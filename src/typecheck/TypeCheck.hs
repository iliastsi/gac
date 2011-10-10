--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Type check our abstruct syntax tree
--
-- The aim of the type checker is to transform from the UnTypedAst type to
-- the TypedAst type, so basicaly
--      typeCheckExpr :: UExpr -> TExpr a
--
-- We will use our TcM State Monad
--      typeCheckExpr :: UExpr -> TcM (TExpr a)
--
-- But wait! This type is totally wrong. Why? Because it promises that given a
-- UExpr the type checker can return any type, i.e., writing out the
-- (normally implicit) quantifier the type is:
--      typeCheckExpr :: forall a . UExpr -> TcM (TExpr a)
-- But this is not the case, the type checker will figure out a type and return
-- an expression with this specific type, so the type we really want is
--      typeCheckExpr :: exists a . UExpr -> TcM (TExpr a)
-- 
-- Haskell doesn't allow this type to be written this way; we need to package
-- up the existential type in a data type. Like so:
--      data AExpr = forall a . AExpr (TExpr a) (TType a)
--
-- Now our typeCheckExpr function has type
--      typeCheckExpr :: UExpr -> TcM (AExpr)
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, PatternGuards #-}

module TypeCheck where

import UnTypedAst
import TypedAst
import TcMonad
import SrcLoc
import SymbolTable

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


-- -------------------------------------------------------------------
-- TypeCheck UExpr

typeCheckExpr :: Located UExpr -> TcM (Located AExpr)
-- UExprInt
typeCheckExpr (L loc (UExprInt i)) = do
    return (L loc $ AExpr (TExprInt (fromIntegral i)) (TTypeInt))
-- UExprChar
typeCheckExpr (L loc (UExprChar c)) = do
    return (L loc $ AExpr (TExprChar (toEnum (fromEnum c))) (TTypeChar))
-- UExprString
typeCheckExpr (L loc (UExprString s)) = do
    return (L loc $ AExpr (TExprString s) (TTypeArray (length s) TTypeChar))
-- UExprVar
typeCheckExpr (L loc (UExprVar v)) = do
    (L _ (AVariable tvar ttype)) <- typeCheckVariable (L loc v)
    return (L loc $ AExpr (TExprVar tvar) ttype)
-- UExprFun
typeCheckExpr(L loc (UExprFun f)) = do
    (L _ (AFuncCall tfun ttype)) <- typeCheckFunc (L loc f)
    return (L loc $ AExpr (TExprFun tfun) ttype)
-- UExprMinus
typeCheckExpr (L loc (UExprMinus luexpr)) = do
    (L teloc (AExpr texpr ttype)) <- typeCheckExpr luexpr
    return (L loc $ AExpr (TExprMinus (L teloc texpr)) ttype)
-- UExprOp
typeCheckExpr luexpr@(L loc (UExprOp lue1 lop lue2)) = do
    (L l1 (AExpr te1 tt1)) <- typeCheckExpr lue1
    (L l2 (AExpr te2 tt2)) <- typeCheckExpr lue2
    let lte1 = L l1 te1
        lte2 = L l2 te2
        unknown_expr = (TExprVar (TVar "unknown" TTypeUnknown))
    if (AType tt1) == (AType TTypeUnknown) || (AType tt2) == (AType TTypeUnknown)
       then return (L loc $ AExpr unknown_expr TTypeUnknown)
       else do
           case test tt1 tt2 of
                Just Eq -> do
                    return (L loc $ AExpr (TExprOp lte1 lop lte2) tt1)
                Nothing -> do
                    tcOpExprErr luexpr (AType tt1) (AType tt2)
                    return (L loc $ AExpr unknown_expr TTypeUnknown)

-- ---------------------------
-- Error when the type of expressions on TExprOp is different
tcOpExprErr :: Located UExpr -> AType -> AType -> TcM ()
tcOpExprErr (L loc uexpr@(UExprOp _ lop _)) ftype stype =
    addTcError loc (UAstE uexpr)
        ("First argument of `" ++ show (unLoc lop) ++ "' is of type `" ++
         show ftype ++ "'\n\tSecond argument of `" ++ show (unLoc lop) ++
         "' is of type `" ++ show stype ++ "'")


-- -------------------------------------------------------------------
-- TypeCheck UVariable

typeCheckVariable :: Located UVariable -> TcM (Located AVariable)
-- UVar
typeCheckVariable (L loc (UVar ide)) = do
    m_var_info <- getVarM (L loc ide)
    ide' <- getVarNameM ide m_var_info
    (AType var_type) <- getVarTypeM m_var_info
    return (L loc $ AVariable (TVar ide var_type) var_type)
-- UVarArray
typeCheckVariable luvar@(L loc (UVarArray lide lexpr)) = do
    (L aeloc (AExpr texpr expr_type)) <- typeCheckExpr lexpr
    m_var_info <- getVarM lide
    lide' <- liftM (L (getLoc lide)) (getVarNameM (unLoc lide) m_var_info)
    (AType var_type) <- getVarTypeM m_var_info
    let exprIsInt     = (AType expr_type) == (AType TTypeInt)
        exprIsUnknown = (AType expr_type) == (AType TTypeUnknown)
        varIsArray    = atypeIsArray (AType var_type)
        varIsUnknown  = (AType var_type)  == (AType TTypeUnknown)
    if (not exprIsInt) && (not exprIsUnknown)
       then tcIntExprErr luvar
       else return ()
    if (not varIsArray) && (not varIsUnknown)
       then tcArrayVarErr luvar (AType var_type)
       else return ()
    if exprIsInt && varIsArray
       then do
           let lexpr' = L aeloc texpr
           case test expr_type TTypeInt of
                Just Eq ->
                    return (L loc $ AVariable (TVarArray lide' var_type lexpr') var_type)
                Nothing ->
                    error "in typeCheckVariable"
       else do
           return (L loc $ AVariable (TVar "unknown" TTypeUnknown) TTypeUnknown)

-- ---------------------------
-- Check if a given AType is of TTypeArray
atypeIsArray :: AType -> Bool
atypeIsArray (AType (TTypeArray _ _)) = True
atypeIsArray _ = False

-- ---------------------------
-- Error when the array index expression is not of type of int
tcIntExprErr :: Located UVariable -> TcM ()
tcIntExprErr (L loc (uvar@(UVarArray lide lexpr))) =
    addTcError loc (UAstV uvar)
        ("Array index `" ++ show (unLoc lexpr) ++ "' has to be of type `int'")

-- Error when variable is not of type `array'
tcArrayVarErr :: Located UVariable -> AType -> TcM ()
tcArrayVarErr (L loc uvar@(UVarArray lide lexpr)) var_type =
    addTcError loc (UAstV uvar)
        ("Incompatible type of variable `" ++ show (unLoc lide) ++
         "'\n\tExpected `array' but variable is of type `" ++
         show var_type ++ "'")


-- -------------------------------------------------------------------
-- TypeCheck UFuncCall

typeCheckFunc :: Located UFuncCall -> TcM (Located AFuncCall)
typeCheckFunc lufunc@(L loc (UFuncCall lide lupars)) = do
    m_fun_info <- getFuncM lide
    AType ret_type <- getFuncRetTypeM m_fun_info
    apar_type <- getFuncParamsM m_fun_info
    if (AType ret_type) /= (AType TTypeUnknown)
       then do
           lapars <- tcFunPar lufunc apar_type
           return (L loc $ AFuncCall (TFuncCall lide ret_type lapars) ret_type)
       else do
           return (L loc $ AFuncCall (TFuncCall lide TTypeUnknown []) TTypeUnknown)

-- ---------------------------
-- Type Check function parameters
tcFunPar :: Located UFuncCall -> [AType] -> TcM [LAExpr]
tcFunPar lufunc@(L loc (UFuncCall lide lupars)) expr_atype = do
    let pars_len = length lupars
        type_len = length expr_atype
    if pars_len /= type_len
       then do
           tcParLenErr lufunc pars_len type_len
           return []
       else do
           lapars <- tcFunPar' (unLoc lide) lupars expr_atype []
           return $ reverse lapars

tcFunPar' :: Ide -> [LUExpr] -> [AType] -> [LAExpr] -> TcM [LAExpr]
tcFunPar' ide [] [] acc = return acc
tcFunPar' ide (pexpr:pexprs) (ptype:ptypes) acc = do
    (L aeloc aexpr@(AExpr texpr ttype)) <- typeCheckExpr pexpr
    if (AType ttype) == ptype
       then return ()
       else tcParTypeErr ide pexpr ((length acc) + 1) ptype (AType ttype)
    tcFunPar' ide pexprs ptypes ((L aeloc aexpr):acc)
tcFunPar' ide _  _  _   = error "in tcFunPar'"

-- ---------------------------
-- Error when the function parameter's number is different from the prototype
tcParLenErr :: Located UFuncCall -> Int -> Int -> TcM ()
tcParLenErr (L loc ufunc@(UFuncCall lide lupars)) pars_len type_len =
    addTcError loc (UAstF ufunc)
        ("The function `" ++ show (unLoc lide) ++ "' is applied to " ++
         show pars_len ++ " parameters but its type has " ++ show type_len)

-- Error when the function parameter's type is different from the prototype
tcParTypeErr :: Ide -> Located UExpr -> Int -> AType -> AType -> TcM ()
tcParTypeErr ide (L loc uexpr) count exptype acttype =
    addTcError loc (UAstE uexpr)
        ("Incompatible type of argument " ++ show count ++ " of function `" ++
         show ide ++"'\n\tExpected `" ++ show exptype ++
         "' but argument is of type `" ++ show acttype ++ "'")
