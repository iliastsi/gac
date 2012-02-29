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

module LambdaLift (lambdaLift) where

import TypedAst


lambdaLift :: TAst -> TAst
lambdaLift = id
