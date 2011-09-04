--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module contains some highly random utility functions
--
--------------------------------------------------------------------------------

module Util (
    -- * Comparisons
    isEqual, thenCmp
  ) where


infixr 9 `thenCmp`

-- -------------------------------------------------------------------
-- Comparisons
isEqual :: Ordering -> Bool
-- Often used in ( isEqual (a `compare` b))
isEqual GT = False
isEqual EQ = True
isEqual LT = False

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering
