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
    let (ide, defs, stmts) = disFunDef adef
        evars' = paramToFreeVar tdef [] ++ evars
        efuns' = (ide, evars) : efuns
        (dvars, dfuns, efuns'') = liftDefs defs ([], [], (evars', efuns'))
        stmts' = liftStmts stmts (efuns'++efuns'')
        adef' = chFunDef adef evars (ide, dvars, stmts')
    in
    (adef':dfuns, (ide, evars))

-- Disassemble a function definition
disFunDef :: ADefFun -> (Ide, [ADef], TStmt)
disFunDef (ADefFun (TDefPar _ atype def) (TTypeFuncR atype' dtype)) =
    case test atype atype' of
         Just Eq -> disFunDef $ ADefFun def dtype
         Nothing -> panic "LambdaLift.disFunDef had to return Eq"
disFunDef (ADefFun (TDefFun ide _ adefs tstmt) _) =
    (ide, adefs, tstmt)
disFunDef _ = panic "LambdaLift.disFunDef got unexpected input"

-- Turn pamaters to FreeVars
paramToFreeVar :: forall a. (TDefFun a) -> [FreeVar] -> [FreeVar]
paramToFreeVar (TDefPar ide atype tdefs) acc =
    if atypeIsPtr (AType atype)
       -- since our variable is already an array return it as it is
       then paramToFreeVar tdefs ((ide, AType atype):acc)
       -- else return a ptr
       else paramToFreeVar tdefs ((ide, AType $ TTypePtr atype):acc)
paramToFreeVar (TDefFun {}) acc = acc
paramToFreeVar _ _ = panic "LambdaLift.paramToFreeVar got unexpected input"

-- Check if a given AType is of TTypePtr
atypeIsPtr :: AType -> Bool
atypeIsPtr (AType (TTypePtr _)) = True
atypeIsPtr _ = False

-- Change the function definition to contain the free variables
chFunDef :: ADefFun -> [FreeVar] -> (Ide, [ADefVar], TStmt) -> ADefFun
chFunDef (ADefFun (TDefPar lide atype def) (TTypeFuncR atype' rtype)) fv finfo =
    case test atype atype' of
         Just Eq ->
             case chFunDef (ADefFun def rtype) fv finfo of
                  ADefFun def' dtype ->
                      ADefFun (TDefPar lide atype def')
                                (TTypeFuncR atype dtype)
         Nothing -> panic "LambdaLift.chFunDef had to return Eq"
chFunDef dfun@(ADefFun (TDefFun {}) _) ((ide,AType itype):fvs) finfo =
    case chFunDef dfun fvs finfo of
         ADefFun def dtype ->
             ADefFun (TDefPar ide itype def)
                    (TTypeFuncR itype dtype)
chFunDef (ADefFun (TDefFun _ atype _ _) rtype) [] (ide, adef, tstmt) =
    ADefFun (TDefFunL ide atype adef tstmt) rtype
chFunDef _ _ _ = panic "LambdaLift.chFunDef got unexpected input"

-- ---------------------------
-- Take a list of ADefs (definitions for one function)
-- and add the variables/functions to the current environment
-- Return first the variables, then the lifted functions
liftDefs :: [ADef] -> ([ADefVar], [ADefFun], Env) -> ([ADefVar], [ADefFun],[FreeFun])
liftDefs ((Right adef@(ADefVar (TDefVar ide atype) _)):adefs) (var_acc, fun_acc, (evars, efuns)) =
    -- return the free variable as an array
    liftDefs adefs (adef:var_acc, fun_acc, ((ide, AType $ TTypePtr atype):evars, efuns))
liftDefs ((Left fundef):adefs) (var_acc, fun_acc, env@(evars, efuns)) =
    let (funs, efun) = lambdaLiftAux fundef env
    in liftDefs adefs (var_acc, funs++fun_acc, (evars, efun:efuns))
liftDefs [] (var_acc, fun_acc, (_,free_fun)) = (var_acc, fun_acc, free_fun)

-- ---------------------------
-- Take a TStmt and replace all function calls with
-- new ones, containing all free variables
-- We add this `free variables' at the end of the parameters
liftStmts :: TStmt -> [FreeFun] -> TStmt
liftStmts (TStmtCompound has_ret tstmts) env =
    TStmtCompound has_ret $ map (\s -> liftStmts s env) tstmts
liftStmts tstmt@(TStmtFun afun@(AFuncCall tfun rtype)) env =
    let fname = getFunName tfun in
    case lookup fname env of
         Just fv -> TStmtFun (liftCall afun (reverse fv) id (ATypeF rtype))
         -- The function is not in scope (probably library function)
         Nothing -> tstmt
liftStmts tstmt _env = tstmt

-- Return the name of the function (given a TFuncCall)
getFunName :: forall a . TFuncCall a -> Ide
getFunName (TParamCall _ _ tfun) = getFunName tfun
getFunName (TFuncCall ide _) = ide

-- ---------------------------
-- Lift a function call
liftCall :: AFuncCall -> [FreeVar] -> (ATypeF -> ATypeF) -> ATypeF -> AFuncCall
liftCall afun ((vname,AType pvtype@(TTypePtr vtype)):fvs) type_fn frtype =
    let param = TExprVar (TVarPtr (TVar vname vtype))
        type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc pvtype rtype)) . type_fn
    in
    case liftCall afun fvs type_fn' frtype of
         AFuncCall rest (TTypeFunc ctype rtype) ->
             case test pvtype ctype of
                  Just Eq -> AFuncCall (TParamCall param pvtype rest) rtype
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall (AFuncCall (TParamCall texpr etype tfun) atype) [] type_fn frtype =
    let type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc etype rtype)) . type_fn
        aftype = TTypeFunc etype atype
        afun' = AFuncCall tfun aftype
    in
    case liftCall afun' [] type_fn' frtype of
         AFuncCall rest (TTypeFunc ctype rtype) ->
             case test etype ctype of
                  Just Eq -> AFuncCall (TParamCall texpr etype rest) rtype
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall (AFuncCall (TFuncCall ide _) _) [] type_fn frtype =
    case type_fn frtype of
         ATypeF ftype -> AFuncCall (TFuncCall ide ftype) ftype
liftCall _ _ _ _ = panic "LambdaLift.liftCall got unexpected input"
