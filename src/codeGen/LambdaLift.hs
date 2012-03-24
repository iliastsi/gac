--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Lambda Lift our nested functions
--
-- All functions and variables have different names (after the typecheking)
-- As a naive impelentation we add EVERY POSIBLE free variable as a
-- function parameter, and not only the ones actually used (TODO)
-- We depend on llvm optimizer to eliminate the extra parameters
-- (and it actually does for any optimization level)
--
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, RankNTypes #-}
module LambdaLift (lambdaLift) where

import TypedAst
import UnTypedAst (Ide)
import Outputable
import SrcLoc


lambdaLift :: TAst -> [TAst]
lambdaLift tast = fst $ lambdaLiftAux tast ([],[])

-- a list of the free variables so far
type FreeVar = (Ide, AType)

-- a list of the functions lifted
-- (along with the vars they lifted)
type FreeFun = (Ide, [FreeVar])

-- State
type Env = ([FreeVar], [FreeFun])

-- ---------------------------
-- Take a Function Definition (in form of ADef) and the Environment
-- and return the new Environment, the Function Definition and
-- a list of Funtions lifted (in form of function definitions)
lambdaLiftAux :: ADefFun -> Env -> ([ADefFun], FreeFun)
lambdaLiftAux adef@(ADefFun tdef _) (evars, efuns) =
    let (lide, defs, stmts) = disFunDef adef
        evars' = paramToFreeVar tdef [] ++ evars
        efuns' = (unLoc lide, evars') : efuns
        (dvars, dfuns) = liftDefs defs ([], [], (evars', efuns'))
        stmts' = liftStmts stmts efuns
        adef' = chFunDef adef evars (lide, dvars, stmts')
    in
    (adef':dfuns, (unLoc lide, evars'))

-- Disassemble a function definition
disFunDef :: ADefFun -> (Located Ide, [ADef], TStmt)
disFunDef (ADefFun (TDefPar _ atype def) (TTypeFunc atype' dtype)) =
    case test atype atype' of
         Just Eq -> disFunDef $ ADefFun def dtype
         Nothing -> panic "LambdaLift.disFunDef had to return Eq"
disFunDef (ADefFun (TDefFun lide _ adefs tstmt) _) =
    (lide, adefs, tstmt)
disFunDef _ = panic "LambdaLift.disFunDef got unexpected input"

-- Turn pamaters to FreeVars
paramToFreeVar :: forall a. (TDefFun a) -> [FreeVar] -> [FreeVar]
paramToFreeVar (TDefPar lide atype tdefs) acc =
    if atypeIsArray (AType atype)
       -- since our variable is already an array return it as it is
       then paramToFreeVar tdefs ((unLoc lide, AType atype):acc)
       -- else return a ptr
       else paramToFreeVar tdefs ((unLoc lide, AType $ TTypePtr atype):acc)
paramToFreeVar (TDefFun {}) acc = acc
paramToFreeVar _ _ = panic "LambdaLift.paramToFreeVar got unexpected input"

-- Check if a given AType is of TTypePtr
atypeIsArray :: AType -> Bool
atypeIsArray (AType (TTypePtr _)) = True
atypeIsArray (AType TTypeArray {}) = True
atypeIsArray _ = False

-- Change the function definition to contain the free variables
chFunDef :: ADefFun -> [FreeVar] -> (Located Ide, [ADefVar], TStmt) -> ADefFun
chFunDef (ADefFun (TDefPar lide atype def) (TTypeFunc atype' rtype)) fv finfo =
    case test atype atype' of
         Just Eq ->
             case chFunDef (ADefFun def rtype) fv finfo of
                  ADefFun def' dtype ->
                      ADefFun (TDefPar lide atype def')
                                (TTypeFunc atype dtype)
         Nothing -> panic "LambdaLift.chFunDef had to return Eq"
chFunDef dfun@(ADefFun (TDefFun {}) _) ((ide,AType itype):fvs) finfo =
    case chFunDef dfun fvs finfo of
         ADefFun def dtype ->
             ADefFun (TDefPar (L wiredInSrcSpan ide) itype def)
                    (TTypeFunc itype dtype)
chFunDef (ADefFun (TDefFun _ atype _ _) rtype) [] (lide, adef, tstmt) =
    ADefFun (TDefFunL lide atype adef tstmt) rtype
chFunDef _ _ _ = panic "LambdaLift.chFunDef got unexpected input"

-- ---------------------------
-- Take a list of ADefs (definitions for one function)
-- and add the variables/functions to the current environment
-- Return first the variables, then the lifted functions
liftDefs :: [ADef] -> ([ADefVar], [ADefFun], Env) -> ([ADefVar], [ADefFun])
liftDefs ((Right adef@(ADefVar (TDefVar lide atype) _)):adefs) (var_acc, fun_acc, (evars, efuns)) =
    -- return the free variable as an array
    liftDefs adefs (adef:var_acc, fun_acc, ((unLoc lide, AType $ TTypePtr atype):evars, efuns))
liftDefs ((Right adef@(ADefVar (TDefArr tdef _) _)):adefs) (var_acc, fun_acc, (evars, efuns)) =
    -- return the free variable as it is (it is already an array)
    let (array, arr_type) = getArrayVar tdef in
    liftDefs adefs (adef:var_acc, fun_acc, ((array, AType $ TTypePtr arr_type):evars, efuns))
liftDefs ((Left fundef):adefs) (var_acc, fun_acc, env@(evars, efuns)) =
    let (funs, efun) = lambdaLiftAux fundef env
    in liftDefs adefs (var_acc, funs++fun_acc, (evars, efun:efuns))
liftDefs [] (var_acc, fun_acc, _) = (var_acc, fun_acc)

-- Return the ide and the type of an array definition
getArrayVar :: TDefVar a -> (Ide, TType a)
getArrayVar (TDefVar lide atype) =
    (unLoc lide, atype)
getArrayVar (TDefArr tdef _) =
    let (array, arr_type) = getArrayVar tdef
    in (array, TTypePtr arr_type)

-- ---------------------------
-- Take a TStmt and replace all function calls with
-- new ones, containing all free variables
-- We add this `free variables' at the end of the parameters
liftStmts :: TStmt -> [FreeFun] -> TStmt
liftStmts (TStmtCompound tstmts) env =
    TStmtCompound $ map (\s -> liftStmts s env) tstmts
liftStmts tstmt@(TStmtFun afun@(AFuncCall tfun rtype)) env =
    let fname = getFunName tfun in
    case lookup fname env of
         Just fv -> TStmtFun (liftCall afun (reverse fv) id (ATypeF rtype))
         -- The function is not in scope (probably library function)
         Nothing -> tstmt
liftStmts tstmt _env = tstmt

-- Return the name of the function (given a TFuncCall)
getFunName :: forall a . TFuncCall a -> Ide
getFunName (TParamCall _ tfun) = getFunName tfun
getFunName (TFuncCall lide _) = unLoc lide

-- ---------------------------
-- Lift a function call
liftCall :: AFuncCall -> [FreeVar] -> (ATypeF -> ATypeF) -> ATypeF -> AFuncCall
liftCall afun ((vname,AType vtype):fvs) type_fn frtype =
    let vtype' = TTypePtr vtype
        param = TExprVar (TVarPtr (TVar vname vtype))
        type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc vtype' rtype)) . type_fn
    in
    case liftCall afun fvs type_fn' frtype of
         AFuncCall rest (TTypeFunc ctype rtype) ->
             case test vtype' ctype of
                  Just Eq -> AFuncCall (TParamCall param rest) rtype
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall (AFuncCall (TParamCall texpr tfun) _) [] type_fn frtype =
    let etype = getExprType texpr
        type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc etype rtype)) . type_fn
        aftype = getFunType tfun
        afun' = AFuncCall tfun aftype
    in
    case liftCall afun' [] type_fn' frtype of
         AFuncCall rest (TTypeFunc ctype rtype) ->
             case test etype ctype of
                  Just Eq -> AFuncCall (TParamCall texpr rest) rtype
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall (AFuncCall (TFuncCall lide _) _) [] type_fn frtype =
    case type_fn frtype of
         ATypeF ftype -> AFuncCall (TFuncCall lide ftype) ftype

-- Return the type of a TExpr
getExprType :: TExpr a -> TType a
getExprType _ = undefined

-- Return the type of a TFuncCall
getFunType :: TFuncCall a -> TType a
getFunType _ = undefined
