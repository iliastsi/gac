--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Type check our abstruct syntax tree
--
-- The aim of the type checker is to transform from the Uast type to
-- the Tast type, so basicaly
--      typeCheckExpr :: UExpr -> TExpr a
--
-- But things can go wrong, so we will use our GAC State Monad
--      typeCheckExpr :: UExpr -> GacMonad (TExpr a)
--
-- But wait! This type is totally wrong. Why? Because it promises that given a
-- UExpr the type checker can return any type, i.e., writing out the
-- (normally implicit) quantifier the type is:
--      typeCheckExpr :: forall a . UExpr -> GacMonad (TExpr a)
-- But this is not the case, the type checker will figure out a type and return
-- an expression with this specific type, so the type we really want is
--      typeCheckExpr :: exists a . UExpr -> GacMonad (TExpr a)
-- 
-- Haskell doesn't allow this type to be written this way; we need to package
-- up the existential type in a data type. Like so:
--      data ATExpr = forall a . TExpr a ::: TTyp a
--
-- Now our typeCheckExpr function has type
--      typeCheckExpr :: UExpr -> GacMonad (ATExpr)
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, PatternGuards #-}

module TypeCheck where

import Uast
import Tast
import GacMonad

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


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


