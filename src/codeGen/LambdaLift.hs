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


lambdaLift :: Located TAst -> [Located TAst]
lambdaLift ltast = fst $ lambdaLiftAux ltast ([],[])

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
lambdaLiftAux :: Located ADef -> Env -> ([Located ADef], FreeFun)
lambdaLiftAux ladef@(L _ adef@(ADef tdef _)) (evars, efuns) =
    let (lide, defs, lstmts) = disFunDef adef
        evars' = paramToFreeVar tdef [] ++ evars
        efuns' = (unLoc lide, evars') : efuns
        (dvars, dfuns) = liftDefs defs ([], [], (evars', efuns'))
        lstmts' = liftStmts lstmts efuns
        ladef' = chFunDef ladef evars (lide, dvars, lstmts')
    in
    (ladef':dfuns, (unLoc lide, evars'))

-- Disassemble a function definition
disFunDef :: ADef -> (Located Ide, [LADef], LTStmt)
disFunDef (ADef (TDefPar _ ltype ldef) (TTypeFunc ltype' dtype)) =
    case test (unLoc ltype) ltype' of
         Just Eq -> disFunDef $ ADef (unLoc ldef) dtype
         Nothing -> panic "LambdaLift.disFunDef had to return Eq"
disFunDef (ADef (TDefFun lide _ ladef ltstmt) _) =
    (lide, ladef, ltstmt)
disFunDef _ = panic "LambdaLift.disFunDef got unexpected input"

-- Turn pamaters to FreeVars
paramToFreeVar :: forall a . Type a => (TDef a) -> [FreeVar] -> [FreeVar]
paramToFreeVar (TDefPar lide ltype ltdefs) acc =
    paramToFreeVar (unLoc ltdefs) ((unLoc lide, AType (unLoc ltype)):acc)
paramToFreeVar (TDefFun {}) acc = acc
paramToFreeVar _ _ = panic "LambdaLift.paramToFreeVar got unexpected input"

-- Change the function definition to contain the free variables
chFunDef :: LADef -> [FreeVar] -> (Located Ide, [LADef], LTStmt) -> LADef
chFunDef (L loc (ADef (TDefPar lide ltype ldef) (TTypeFunc ltype' rtype))) fv finfo =
    case test (unLoc ltype)  ltype' of
         Just Eq ->
             case chFunDef (L (getLoc ldef) $ ADef (unLoc ldef) rtype) fv finfo of
                  (L dloc (ADef def dtype)) ->
                      (L loc $ ADef (TDefPar lide ltype (L dloc def))
                                (TTypeFunc (unLoc ltype) dtype))
         Nothing -> panic "LambdaLift.chFunDef had to return Eq"
chFunDef dfun@(L _ (ADef (TDefFun {}) _)) ((ide,AType itype):fvs) finfo =
    case chFunDef dfun fvs finfo of
         (L dloc (ADef def dtype)) ->
             (L wiredInSrcSpan $ ADef (TDefPar (L wiredInSrcSpan ide)
                    (L wiredInSrcSpan itype) (L dloc def)) (TTypeFunc itype dtype))
chFunDef (L loc (ADef (TDefFun _ ltype _ _) rtype)) [] (lide, ladef, ltstmt) =
    L loc $ ADef (TDefFun lide ltype ladef ltstmt) rtype
chFunDef _ _ _ = panic "LambdaLift.chFunDef got unexpected input"

-- ---------------------------
-- Take a list of ADefs (definitions for one function)
-- and add the variables/functions to the current environment
-- Return first the variables, then the lifted functions
liftDefs :: [LADef] -> ([LADef], [LADef], Env) -> ([LADef], [LADef])
liftDefs ((ladef@(L _ (ADef (TDefVar lide ltype) _))):ladefs) (var_acc, fun_acc, (evars, efuns)) =
    liftDefs ladefs (ladef:var_acc, fun_acc, ((unLoc lide, AType (unLoc ltype)):evars, efuns))
liftDefs ((ladef@(L _ (ADef (TDefArr ltdef _) _))):ladefs) (var_acc, fun_acc, (evars, efuns)) =
    let (array, arr_type) = getArrayVar ltdef in
    liftDefs ladefs (ladef:var_acc, fun_acc, ((array, AType $ TTypePtr arr_type):evars, efuns))
liftDefs (lfundef:ladefs) (var_acc, fun_acc, env@(evars, efuns)) =
    let (lfuns, efun) = lambdaLiftAux lfundef env
    in liftDefs ladefs (var_acc, lfuns++fun_acc, (evars, efun:efuns))
liftDefs [] (var_acc, fun_acc, _) = (var_acc, fun_acc)

-- Return the ide and the type of an array definition
getArrayVar :: Located (TDef a) -> (Ide, TType a)
getArrayVar (L _ (TDefVar lide ltype)) =
    (unLoc lide, unLoc ltype)
getArrayVar (L _ (TDefArr ltdef _)) =
    let (array, arr_type) = getArrayVar ltdef
    in (array, TTypePtr arr_type)
getArrayVar _ = panic "LambdaLift.getArrayVar got unexpected input"

-- ---------------------------
-- Take a TStmt and replace all function calls with
-- new ones, containing all free variables
-- We add this `free variables' at the end of the parameters
liftStmts :: LTStmt -> [FreeFun] -> LTStmt
liftStmts (L loc (TStmtCompound ltstmts)) env =
    L loc (TStmtCompound $ map (\s -> liftStmts s env) ltstmts)
liftStmts ltstmt@(L loc (TStmtFun lafun@(L _ (AFuncCall tfun rtype)))) env =
    let fname = getFunName tfun in
    case lookup fname env of
         Just fv -> (L loc (TStmtFun (liftCall lafun (reverse fv) id (AType rtype))))
         -- The function is not in scope (probably library function)
         Nothing -> ltstmt
liftStmts ltstmt _env = ltstmt

-- Return the name of the function (given a TFuncCall)
getFunName :: forall a . TFuncCall a -> Ide
getFunName (TParamCall _ ltfun) = getFunName (unLoc ltfun)
getFunName (TFuncCall lide _) = unLoc lide

-- ---------------------------
-- Lift a function call
liftCall :: LAFuncCall -> [FreeVar] -> (AType -> AType) -> AType -> LAFuncCall
liftCall lafun ((vname,AType vtype):fvs) type_fn frtype =
    let vtype' = TTypePtr vtype
        param = TExprVar (TVarPtr (TVar vname vtype))
        type_fn' = (\(AType rtype) -> AType (TTypeFunc vtype' rtype)) . type_fn
    in
    case liftCall lafun fvs type_fn' frtype of
         (L rest_loc (AFuncCall rest (TTypeFunc ctype rtype))) ->
             case test vtype' ctype of
                  Just Eq -> (L wiredInSrcSpan $ AFuncCall
                        (TParamCall param (L rest_loc rest)) rtype)
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall (L loc (AFuncCall (TParamCall texpr ltfun) _)) [] type_fn frtype =
    let etype = getExprType texpr
        type_fn' = (\(AType rtype) -> AType (TTypeFunc etype rtype)) . type_fn
        laftype = getFunType (unLoc ltfun)
        lafun' = (L (getLoc ltfun) (AFuncCall (unLoc ltfun) laftype))
    in
    case liftCall lafun' [] type_fn' frtype of
         (L rest_loc (AFuncCall rest (TTypeFunc ctype rtype))) ->
             case test etype ctype of
                  Just Eq -> (L loc $ AFuncCall
                        (TParamCall texpr (L rest_loc rest)) rtype)
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall (L loc (AFuncCall (TFuncCall lide _) _)) [] type_fn frtype =
    case type_fn frtype of
         AType ftype -> (L loc $ AFuncCall (TFuncCall lide ftype) ftype)

-- Return the type of a TExpr
getExprType :: (Type a) => TExpr a -> TType a
getExprType _ = theType

-- Return the type of a TFuncCall
getFunType :: (Type a) => TFuncCall a -> TType a
getFunType _ = theType
