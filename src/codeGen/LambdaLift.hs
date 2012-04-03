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
import TcMonad (Prototype)
import Util


lambdaLift :: [Prototype] -> TAst -> [TAst]
lambdaLift protos tast = fst $ lambdaLiftAux protos tast ([],[])

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
lambdaLiftAux :: [Prototype] -> ADefFun -> Env -> ([ADefFun], Maybe FreeFun)
lambdaLiftAux protos adef@(ADefFun tdef _) (evars, efuns) =
    case disFunDef adef of
         Right (ide, (fun_defs, var_defs), stmts) ->
             let evars' = paramToFreeVar tdef [] ++ evars
                 efuns' = (ide, evars) : efuns
                 evars'' = liftVarDefs var_defs evars'
                 (fun_defs', efuns'') = liftFunDefs protos fun_defs ([], (evars'', efuns'))
                 stmts' = liftStmts protos efuns'' stmts
                 adef' = chFunDef adef evars (ide, var_defs, stmts')
             in
             (adef':fun_defs', Just (ide, evars))
         Left ide ->
             case protoNotDefined protos ide of
                  Just ide' -> let adef' = chProtoDef adef ide'
                               in ([adef'], Nothing)
                  Nothing -> ([], Nothing)

-- Check if a prototype is defined or is external
protoNotDefined :: [Prototype] -> Ide -> Maybe Ide
protoNotDefined protos ide =
    case lookupWith (\(_, _, changed_name) -> changed_name==ide) protos of
         Just (_, ide', _) -> Just ide'
         Nothing -> Nothing

-- Disassemble a function definition
disFunDef :: ADefFun -> Either Ide (Ide, ([ADefFun], [ADefVar]), TStmt)
disFunDef (ADefFun (TDefPar _ atype def) (TTypeFuncR atype' dtype)) =
    case test atype atype' of
         Just Eq -> disFunDef $ ADefFun def dtype
         Nothing -> panic "LambdaLift.disFunDef had to return Eq"
disFunDef (ADefFun (TDefFun ide _ adefs tstmt) _) =
    Right (ide, adefs, tstmt)
disFunDef (ADefFun (TDefProt ide _) _) = Left ide
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
    ADefFun (TDefFun ide atype ([],adef) tstmt) rtype
chFunDef _ _ _ = panic "LambdaLift.chFunDef got unexpected input"

-- Change the name of the prototype definition
chProtoDef :: ADefFun -> Ide -> ADefFun
chProtoDef (ADefFun (TDefPar ide atype def) (TTypeFuncR atype' rtype)) pname =
    case test atype atype' of
         Just Eq ->
             case chProtoDef (ADefFun def rtype) pname of
                  ADefFun def' dtype ->
                      ADefFun (TDefPar ide atype def')
                                (TTypeFuncR atype dtype)
         Nothing -> panic "LambdaLift.chProtoDef had to return Eq"
chProtoDef (ADefFun (TDefProt _ide ptype) atype) pname =
    ADefFun (TDefProt pname ptype) atype
chProtoDef _ _ = panic "LambdaLift.chProtoDef got unexpected input"

-- ---------------------------
-- Take a list of ADefVars (variable definitions for one function)
-- and add the variables to the current environment
-- Return the new environment
liftVarDefs :: [ADefVar] -> [FreeVar] -> [FreeVar]
liftVarDefs ((ADefVar (TDefVar ide atype) _):adefs) evars =
    -- return the free variable as an array
    liftVarDefs adefs ((ide, AType $ TTypePtr atype):evars)
liftVarDefs [] evars = evars

-- ---------------------------
-- Take a list of ADefFuns (function definitions for one function)
-- and lift the functions using the current environment
-- Return the lifted functions and the new environment
liftFunDefs :: [Prototype] -> [ADefFun] -> ([ADefFun], Env) -> ([ADefFun], [FreeFun])
liftFunDefs protos (adef:adefs) (fun_acc, env@(evars, efuns)) =
    case lambdaLiftAux protos adef env of
         (fun_defs, Just efun) ->
             liftFunDefs protos adefs (fun_defs++fun_acc, (evars, efun:efuns))
         (fun_defs, Nothing) ->
             liftFunDefs protos adefs (fun_defs++fun_acc, (evars, efuns))
liftFunDefs _ [] (fun_acc, (_,efuns)) = (fun_acc, efuns)

-- ---------------------------
-- Take a TStmt and replace all function calls with
-- new ones, containing all free variables
-- We add this `free variables' at the end of the parameters
liftStmts :: [Prototype] -> [FreeFun] -> TStmt -> TStmt
-- TStmtNothing
liftStmts _protos _env TStmtNothing = TStmtNothing
-- TStmtAssign
liftStmts protos env (TStmtAssign avar aexpr) =
    TStmtAssign avar (liftExprs protos env aexpr)
-- TStmtCompound
liftStmts protos env (TStmtCompound has_ret tstmts) =
    TStmtCompound has_ret $ map (liftStmts protos env) tstmts
-- TStmtFun
liftStmts protos env (TStmtFun afun@(AFuncCall tfun rtype)) =
    let fname = getFunName tfun in
    case lookup fname env of
         Just fv -> TStmtFun (liftCall (protos,env) afun Nothing (reverse fv) id (ATypeF rtype))
         -- The function is not in scope
         Nothing ->
             -- check if function is external
             case lookupWith (\(_, _, cn) -> cn==fname) protos of
                  -- it is external (change it's name)
                  Just (_,ename,_) ->
                      TStmtFun (liftCall (protos,env) afun (Just ename) [] id (ATypeF rtype))
                  -- probably a library function
                  Nothing ->
                      TStmtFun (liftCall (protos,env) afun Nothing [] id (ATypeF rtype))
-- TStmtIf
liftStmts protos env (TStmtIf acond if_stmt else_stmt) =
    let if_stmt' = liftStmts protos env if_stmt
        else_stmt' = case else_stmt of
                          Just es -> Just $ liftStmts protos env es
                          Nothing -> Nothing
        acond' = liftConds protos env acond
    in TStmtIf acond' if_stmt' else_stmt'
-- TStmtWhile
liftStmts protos env (TStmtWhile acond tstmt) =
    let acond' = liftConds protos env acond in
    TStmtWhile acond' (liftStmts protos env tstmt)
-- TStmtReturn
liftStmts protos env (TStmtReturn maexpr) =
    let maexpr' = case maexpr of
                       Just aexpr -> Just $ liftExprs protos env aexpr
                       Nothing -> Nothing
    in TStmtReturn maexpr'

-- ---------------------------
-- Lift Expressions
liftExprs :: [Prototype] -> [FreeFun] -> AExpr -> AExpr
-- TExprFun
liftExprs protos env (AExpr (TExprFun tfun) rtype) =
    let rtype' = TTypeRetIO rtype
        afun = AFuncCall tfun rtype'
        fname = getFunName tfun
    in
    case lookup fname env of
         Just fv ->
             case liftCall (protos,env) afun Nothing (reverse fv) id (ATypeF rtype') of
                  AFuncCall tfun' rtype'' ->
                      case test (TTypeRetIO rtype) rtype'' of
                           Just Eq ->
                               AExpr (TExprFun tfun') rtype
                           Nothing ->
                               panic "LambdaLift.liftExprs test had to return Eq"
         Nothing ->
             -- check if function is external
             let fname' = case lookupWith (\(_, _, cn) -> cn==fname) protos of
                               -- it is external (change it's name)
                               Just (_,ename,_) -> Just ename
                               -- probably a library function
                               Nothing -> Nothing
             in
             case liftCall (protos,env) afun fname' [] id (ATypeF rtype') of
                  AFuncCall tfun' rtype'' ->
                      case test (TTypeRetIO rtype) rtype'' of
                           Just Eq ->
                               AExpr (TExprFun tfun') rtype
                           Nothing ->
                               panic "LambdaLift.liftExprs test had to return Eq"
-- TExprMinus
liftExprs protos env (AExpr (TExprMinus texpr) ttype) =
    case liftExprs protos env (AExpr texpr ttype) of
         AExpr texpr' ttype' ->
             case test ttype ttype' of
                  Just Eq ->
                      AExpr (TExprMinus texpr') ttype
                  Nothing ->
                      panic "LambdaLift.liftExprs test had to return Eq"
-- TExprOp
liftExprs protos env (AExpr (TExprOp te1 op te2 tte) _) =
    case (liftExprs protos env (AExpr te1 tte), liftExprs protos env (AExpr te2 tte)) of
         (AExpr te1' tt1, AExpr te2' tt2) ->
             case (test tt1 tte, test tt2 tte) of
                  (Just Eq, Just Eq) ->
                      AExpr (TExprOp te1' op te2' tte) tte
                  _ -> panic "LambdaLift.liftExprs test had to return Eq"
-- Otherwise
liftExprs _protos _env aexpr = aexpr

-- ---------------------------
-- Lift Conditions
liftConds :: [Prototype] -> [FreeFun] -> ACond -> ACond
-- TCondOp
liftConds protos env (ACond (TCondOp te1 op te2 tte)) =
    case (liftExprs protos env (AExpr te1 tte), liftExprs protos env (AExpr te2 tte)) of
         (AExpr te1' tt1, AExpr te2' tt2) ->
             case (test tt1 tte, test tt2 tte) of
                  (Just Eq, Just Eq) ->
                      ACond (TCondOp te1' op te2' tte)
                  _ -> panic "LambdaLift.liftConds test had to return Eq"
-- TCondLog
liftConds protos env (ACond (TCondLog tc1 op tc2)) =
    case (liftConds protos env (ACond tc1), liftConds protos env (ACond tc2 )) of
         (ACond tc1', ACond tc2') ->
             ACond (TCondLog tc1' op tc2')
-- Otherwise
liftConds _protos _env acond = acond

-- -------
-- Return the name of the function (given a TFuncCall)
getFunName :: forall a . TFuncCall a -> Ide
getFunName (TParamCall _ _ tfun) = getFunName tfun
getFunName (TFuncCall ide _) = ide

-- ---------------------------
-- Lift a function call
liftCall :: ([Prototype], [FreeFun]) -> AFuncCall -> Maybe Ide -> [FreeVar]
         -> (ATypeF -> ATypeF) -> ATypeF -> AFuncCall
liftCall protos afun fname ((vname,AType pvtype@(TTypePtr vtype)):fvs) type_fn frtype =
    let param = TExprVar (TVarPtr (TVar vname vtype))
        type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc pvtype rtype)) . type_fn
    in
    case liftCall protos afun fname fvs type_fn' frtype of
         AFuncCall rest (TTypeFunc ctype rtype) ->
             case test pvtype ctype of
                  Just Eq -> AFuncCall (TParamCall param pvtype rest) rtype
                  Nothing ->
                      panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall pes@(protos, env) (AFuncCall (TParamCall texpr etype tfun) atype) fname [] type_fn frtype =
    let type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc etype rtype)) . type_fn
        aftype = TTypeFunc etype atype
        afun' = AFuncCall tfun aftype
    in
    case (liftExprs protos env (AExpr texpr etype), liftCall pes afun' fname [] type_fn' frtype) of
         (AExpr texpr' etype', AFuncCall rest (TTypeFunc ctype rtype)) ->
             case (test etype' etype, test etype ctype) of
                  (Just Eq, Just Eq) ->
                      AFuncCall (TParamCall texpr' etype rest) rtype
                  _ -> panic "test in LambdaLift.liftCall had to return Eq"
         _ -> panic "case in LambdaLift.liftCall had to return AFuncCall"
liftCall _protos (AFuncCall (TFuncCall ide _) _) Nothing [] type_fn frtype =
    case type_fn frtype of
         ATypeF ftype -> AFuncCall (TFuncCall ide ftype) ftype
liftCall _protos (AFuncCall (TFuncCall _ _) _) (Just ide) [] type_fn frtype =
    case type_fn frtype of
         ATypeF ftype -> AFuncCall (TFuncCall ide ftype) ftype
liftCall _ _ _ _ _ _ = panic "LambdaLift.liftCall got unexpected input"
