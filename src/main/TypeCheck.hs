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


typeCheckExpr :: UExpr -> GacMonad (ATExpr)
typeCheckExpr (UExprInt i)  = return $ TExprInt (fromIntegral i)  ::: TTypeInt
typeCheckExpr (UExprChar c) = return $ TExprChar (toEnum $ fromEnum c) ::: TTypeChar --find smt else
typeCheckExpr (UExprString s) = return $ TExprString s ::: (TTypeArray TTypeChar)
typeCheckExpr (UExprVal v)  =
    case v of
         UVal ide          -> do (AType t) <- getVarTypeM ide
                                 return $ TExprVal ide t ::: t
         UValArray ide off -> do (TExprInt i) ::: toff' <- typeCheckExpr off -- sure Int32
                                 (AType (TTypeArray t)) <- getVarTypeM ide  -- check for errors
                                 return $ TExprValArr ide (TTypeArray t) (TExprInt i) ::: t
typeCheckExpr (UExprPar p) = typeCheckExpr p
typeCheckExpr (UExprFun (UFunCall i pars)) = do aexprs <- mapM typeCheckExpr pars
                                                let ptypes = map (\x -> let (e ::: _) = x in e) aexprs
                                                (AType rett) <- getFuncRetTypeM i
                                                return $ TExprFun i rett ptypes ::: rett
