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

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


typeCheckExpr :: Located UExpr -> TcM (AExpr)
typeCheckExpr (L span (UExprInt i)) = do
    return $ AExpr (TExprInt (fromIntegral i)) (TTypeInt)
typeCheckExpr (L span (UExprChar c)) = do
    return $ AExpr (TExprChar (toEnum (fromEnum c))) (TTypeChar)
typeCheckExpr (L span (UExprString s)) = do
    return $ AExpr (TExprString s) (TTypeArray (length s) TTypeChar)
typeCheckExpr (L span (UExprVar (UVar ide))) = do
    setSrcSpan span
    AType var_type <- liftM fromUType (getVarTypeM ide)
    ide' <- getVarNameM ide
    return $ AExpr (TExprVar (TVar ide' var_type)) var_type
typeCheckExpr uexpr@(L span (UExprVar (UVarArray lide lexpr))) = do
    expr_type <- typeCheckExpr lexpr
    setSrcSpan span
    AType var_type <- liftM fromUType (getVarTypeM (unLoc lide))
    lide' <- liftM (sL (getLoc lide)) (getVarNameM (unLoc lide))
    case expr_type of
         AExpr e' TTypeInt -> do
             let lexpr' = sL (getLoc lexpr) e'
             return $ AExpr (TExprVar (TVarArray lide' var_type lexpr')) var_type
         otherwise  -> do
             tcIntExprErr uexpr
             let lexpr' = sL noSrcSpan (TExprInt 0)
             return $ AExpr (TExprVar (TVarArray lide' var_type lexpr')) var_type
typeCheckExpr (L span (UExprFun (UFuncCall lide [lupar]))) = do
    ret_type <- liftM fromUType (getFuncRetTypeM (unLoc lide))
    par_type <- liftM (map fromUType) (getFuncParamsM (unLoc lide))
    return $ AExpr (TExprInt 0) TTypeInt


-- Error when the array index expression is not of type of int
tcIntExprErr :: Located UExpr -> TcM ()
tcIntExprErr luexpr@(L span (uexpr@(UExprVar (UVarArray lide lexpr)))) = do
    addTcError span (Just uexpr)
                ("Array index `" ++ show (unLoc lexpr) ++ "' has to be of type of `int'")

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` a `seq` L span a

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
