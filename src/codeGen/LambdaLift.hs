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
                 stmts' = liftStmts protos stmts efuns''
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
liftStmts :: [Prototype] -> TStmt -> [FreeFun] -> TStmt
liftStmts protos (TStmtCompound has_ret tstmts) env =
    TStmtCompound has_ret $ map (\s -> liftStmts protos s env) tstmts
liftStmts protos tstmt@(TStmtFun afun@(AFuncCall tfun rtype)) env =
    let fname = getFunName tfun in
    case lookup fname env of
         Just fv -> TStmtFun (liftCall afun (reverse fv) id (ATypeF rtype))
         -- The function is not in scope
         Nothing ->
             -- check if function is external
             case lookupWith (\(_, _, cn) -> cn==fname) protos of
                  -- it is external (change it's name)
                  Just (_,ename,_) -> TStmtFun $ AFuncCall (changeCallName tfun ename) rtype
                  -- probably a library function
                  Nothing -> tstmt
liftStmts _ tstmt _env = tstmt

-- Return the name of the function (given a TFuncCall)
getFunName :: forall a . TFuncCall a -> Ide
getFunName (TParamCall _ _ tfun) = getFunName tfun
getFunName (TFuncCall ide _) = ide

-- ---------------------------
-- Change the name of an external function
-- in a function call
changeCallName :: TFuncCall a -> Ide -> TFuncCall a
changeCallName (TParamCall texpr etype tfun) fname =
    TParamCall texpr etype $ changeCallName tfun fname
changeCallName (TFuncCall _ide ttype) fname =
    TFuncCall fname ttype

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
