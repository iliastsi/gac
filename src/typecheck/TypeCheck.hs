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


typeCheckExpr :: Located UExpr -> TcM (Located AExpr)
typeCheckExpr (L loc (UExprInt i)) = do
    return (L loc $ AExpr (TExprInt (fromIntegral i)) (TTypeInt))
typeCheckExpr (L loc (UExprChar c)) = do
    return (L loc $ AExpr (TExprChar (toEnum (fromEnum c))) (TTypeChar))
typeCheckExpr (L loc (UExprString s)) = do
    return (L loc $ AExpr (TExprString s) (TTypeArray (length s) TTypeChar))
typeCheckExpr (L loc (UExprVar (UVar ide))) = do
    let lide = L loc ide
    m_var_info <- getVarM lide
    ide' <- getVarNameM ide m_var_info
    (AType var_type) <- getVarTypeM m_var_info
    return (L loc $ AExpr (TExprVar (TVar ide' var_type)) var_type)
typeCheckExpr luexpr@(L loc (UExprVar (UVarArray lide lexpr))) = do
    (L aeloc aexpr) <- typeCheckExpr lexpr
    m_var_info <- getVarM lide
    lide' <- liftM (L (getLoc lide)) (getVarNameM (unLoc lide) m_var_info)
    (AType var_type) <- getVarTypeM m_var_info
    case aexpr of
         AExpr e' TTypeInt -> do
             let lexpr' = L aeloc e'
             return (L loc $ AExpr (TExprVar (TVarArray lide' var_type lexpr')) var_type)
         otherwise  -> do
             tcIntExprErr luexpr
             let lexpr' = L noSrcSpan (TExprInt 0)
             return (L loc $ AExpr (TExprVar (TVarArray lide' var_type lexpr')) var_type)
typeCheckExpr luexpr@(L loc (UExprFun (UFuncCall lide lupars))) = do
    m_fun_info <- getFuncM lide
    AType ret_type <- getFuncRetTypeM m_fun_info
    apar_type <- getFuncParamsM m_fun_info
    if (AType ret_type) /= (AType TTypeUnknown)
       then do
           lapars <- tcFunPar luexpr apar_type
           return (L loc $ AExpr (TExprFun lide ret_type lapars) ret_type)
       else
           return (L loc $ AExpr (TExprFun lide TTypeUnknown []) TTypeUnknown)
typeCheckExpr (L loc (UExprMinus luexpr)) = do
    (L teloc (AExpr texpr ttype)) <- typeCheckExpr luexpr
    return (L loc $ AExpr (TExprMinus (L teloc texpr)) ttype)
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



-- Type Check function parameters
tcFunPar :: Located UExpr -> [AType] -> TcM [LAExpr]
tcFunPar luexpr@(L loc (UExprFun (UFuncCall lide lupars))) expr_atype = do
    let pars_len = length lupars
        type_len = length expr_atype
    if pars_len /= type_len
       then do
           tcParLenErr luexpr pars_len type_len
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

-- Error when the function parameter's number is different from the prototype
tcParLenErr :: Located UExpr -> Int -> Int -> TcM ()
tcParLenErr (L loc uexpr@(UExprFun (UFuncCall lide lupars))) pars_len type_len =
    addTcError loc (Just uexpr)
                ("The function `" ++ show (unLoc lide) ++ "' is applied to " ++
                 show pars_len ++ " parameters but its type has " ++ show type_len)

-- Error when the function parameter's type is different from the prototype
tcParTypeErr :: Ide -> Located UExpr -> Int -> AType -> AType -> TcM ()
tcParTypeErr ide (L loc uexpr) count exptype acttype =
    addTcError loc (Just uexpr)
                ("Incompatible type of argument " ++ show count ++ " of `" ++
                 show ide ++"'\n\tExpected `" ++ show exptype ++
                 "' but argument is of type `" ++ show acttype ++ "'")

-- Error when the array index expression is not of type of int
tcIntExprErr :: Located UExpr -> TcM ()
tcIntExprErr (L loc (uexpr@(UExprVar (UVarArray lide lexpr)))) =
    addTcError loc (Just uexpr)
                ("Array index `" ++ show (unLoc lexpr) ++ "' has to be of type `int'")

-- Error when the type of expressions on TExprOp is different
tcOpExprErr :: Located UExpr -> AType -> AType -> TcM ()
tcOpExprErr (L loc uexpr@(UExprOp _ lop _)) ftype stype =
    addTcError loc (Just uexpr)
                ("First argument of `" ++ show (unLoc lop) ++ "' is of type `" ++
                 show ftype ++ "'\n\tSecond argument of `" ++ show (unLoc lop) ++
                 "' is of type `" ++ show stype ++ "'")

-- ---------------------------
{-
typeCheckExpr :: UExpr -> GacMonad (ATExpr)
typeCheckExpr (UExprInt i)  = return $ TExprInt (fromIntegral i)  ::: TTypeInt
typeCheckExpr (UExprChar c) = return $ TExprChar (toEnum $ fromEnum c) ::: TTypeChar --find smt else
typeCheckExpr (UExprString s) = return $ TExprString s ::: (TTypeArray TTypeChar)
typeCheckExpr (UExprVal v)  =
    case v of
         UVal ide          -> do (_ ::: t) <- getVarTypeM ide
                                 return $ TExprVal ide t ::: t
         UValArray ide off -> do (TExprInt i) ::: toff' <- typeCheckExpr off -- sure Int32
                                 (_ ::: (TTypeArray t)) <- getVarTypeM ide  -- check for errors
                                 return $ TExprValArr ide (TTypeArray t) (TExprInt i) ::: t
typeCheckExpr (UExprPar p) = typeCheckExpr p
typeCheckExpr (UExprFun (UFunCall i pars)) =
    do uapars <- mapM typeCheckExpr pars
       tapars <- getFuncParamsM i
       when (checkPars uapars tapars) (addErrorM "in parameters")
       (_ ::: rett) <- getFuncRetTypeM i
       return $ TExprFun i rett uapars ::: rett
    where checkPars :: [ATExpr] -> [ATExpr] -> Bool
          checkPars ((e1:::t1):l1) ((e2:::t2):l2) =
              case test t1 t2 of
                   Just Eq -> checkPars l1 l2
                   Nothing -> False
typeCheckExpr (UExprSign op e) =
    do (te ::: t) <- typeCheckExpr e
       case test t TTypeInt of
            Just Eq -> return $ TExprSign op te ::: t
            Nothing -> do addErrorM "in ExprSign"
                          return $ TExprSign op (TExprInt 42) ::: TTypeInt
typeCheckExpr (UExprOp e1 op e2) =
    do (te1 ::: t1) <- typeCheckExpr e1
       (te2 ::: t2) <- typeCheckExpr e2
       case test t1 TTypeInt of
            Nothing -> do addErrorM "in ExprOp"
                          return $ TExprOp (TExprInt 42) op (TExprInt 42) ::: TTypeInt
            Just Eq -> case test t2 TTypeInt of
                            Nothing -> do addErrorM "in ExprOp"
                                          return $ TExprOp (TExprInt 42) op (TExprInt 42) ::: TTypeInt
                            Just Eq -> return $ TExprOp te1 op te2 ::: TTypeInt

-}
