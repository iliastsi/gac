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


typeCheckExpr :: Located UExpr -> TcM (AExpr)
typeCheckExpr (L loc (UExprInt i)) = do
    return $ AExpr (TExprInt (fromIntegral i)) (TTypeInt)
typeCheckExpr (L loc (UExprChar c)) = do
    return $ AExpr (TExprChar (toEnum (fromEnum c))) (TTypeChar)
typeCheckExpr (L loc (UExprString s)) = do
    return $ AExpr (TExprString s) (TTypeArray (length s) TTypeChar)
typeCheckExpr (L loc (UExprVar (UVar ide))) = do
    let lide = L loc ide
    m_var_info <- getVarM lide
    ide' <- getVarNameM ide m_var_info
    (AType var_type) <- getVarTypeM m_var_info
    return $ AExpr (TExprVar (TVar ide' var_type)) var_type
typeCheckExpr luexpr@(L loc (UExprVar (UVarArray lide lexpr))) = do
    aexpr <- typeCheckExpr lexpr
    m_var_info <- getVarM lide
    lide' <- liftM (L (getLoc lide)) (getVarNameM (unLoc lide) m_var_info)
    (AType var_type) <- getVarTypeM m_var_info
    case aexpr of
         AExpr e' TTypeInt -> do
             let lexpr' = L (getLoc lexpr) e'
             return $ AExpr (TExprVar (TVarArray lide' var_type lexpr')) var_type
         otherwise  -> do
             tcIntExprErr luexpr
             let lexpr' = L noSrcSpan (TExprInt 0)
             return $ AExpr (TExprVar (TVarArray lide' var_type lexpr')) var_type
typeCheckExpr luexpr@(L loc (UExprFun (UFuncCall lide lupars))) = do
    m_fun_info <- getFuncM lide
    AType ret_type <- getFuncRetTypeM m_fun_info
    apar_type <- getFuncParamsM m_fun_info
    if (AType ret_type) /= (AType TTypeUnknown)
       then do
           lapars <- tcFunPar luexpr apar_type
           return $ AExpr (TExprFun lide ret_type lapars) ret_type
       else
           return $ AExpr (TExprFun lide TTypeUnknown []) TTypeUnknown
typeCheckExpr (L loc (UExprMinus luexpr)) = do
    AExpr texpr ttype <- typeCheckExpr luexpr
    return $ AExpr (TExprMinus (L (getLoc luexpr) texpr)) ttype
typeCheckExpr (L loc (UExprOp lue1 lop lue2)) = do
    AExpr te1 tt1 <- typeCheckExpr lue1
    AExpr te2 tt2 <- typeCheckExpr lue2
    let lte1 = L (getLoc lue1) te1
        lte2 = L (getLoc lue2) te2
        unknown_expr = (TExprVar (TVar "unknown" TTypeUnknown))
    if (AType tt1) == (AType TTypeUnknown) || (AType tt2) == (AType TTypeUnknown)
       then return $ AExpr unknown_expr TTypeUnknown
       else do
           case test tt1 tt2 of
                Just Eq -> return $ AExpr (TExprOp lte1 lop lte2) tt1
                Nothing -> return $ AExpr unknown_expr TTypeUnknown



-- Type Check function parameters
tcFunPar :: Located UExpr -> [AType] -> TcM [LAExpr]
tcFunPar luexpr@(L loc (UExprFun (UFuncCall lide lupars))) expr_atype = do
    let pars_len = length lupars
        type_len = length expr_atype
    lapars <- tcFunPar' (unLoc lide) lupars expr_atype []
    case lapars of
         (True,  ret) -> return $ reverse ret
         (False, _)   -> do
             tcParLenErr luexpr pars_len type_len
             return []

tcFunPar' :: Ide -> [LUExpr] -> [AType] -> [LAExpr] -> TcM (Bool, [LAExpr])
tcFunPar' ide [] [] acc = return (True, acc)
tcFunPar' ide [] _  _   = return (False, [])
tcFunPar' ide _  [] _   = return (False, [])
tcFunPar' ide (pexpr:pexprs) (ptype:ptypes) acc = do
    aexpr@(AExpr texpr ttype) <- typeCheckExpr pexpr
    if (AType ttype) == ptype
       then return ()
       else tcParTypeErr ide pexpr ((length acc) + 1) ptype (AType ttype)
    tcFunPar' ide pexprs ptypes ((L (getLoc pexpr) aexpr):acc)

tcParLenErr :: Located UExpr -> Int -> Int -> TcM ()
tcParLenErr (L loc uexpr@(UExprFun (UFuncCall lide lupars))) pars_len type_len =
    addTcError loc (Just uexpr)
                ("The function `" ++ show (unLoc lide) ++ "' is applied to " ++
                 show pars_len ++ " parameters but its type has " ++ show type_len)

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
