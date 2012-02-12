--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Utilities related to Monad and Applicative classes
-- Mostly for backwards compatibility
--
--------------------------------------------------------------------------------

module MonadUtils (
    mapAccumLM, concatMapM,
    anyM, allM,
    foldlM, foldlM_, foldrM
  ) where

import Control.Monad

-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
           => (acc -> x -> m (acc, y)) -- ^ combining function
           -> acc                      -- ^ initial state
           -> [x]                      -- ^ inputs
           -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return (s2, x' : xs')

-- | Monadic version of conactMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | Monadic version of 'any', aborts the computation at the first @True@ value
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = return False
anyM f (x:xs) = do b <- f x
                   if b then return True
                        else anyM f xs

-- | Monad version of 'all', aborts the computatino at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM f (b:bs) = (f b) >>= (\bv -> if bv then allM f bs else return False)

-- | Monadic version of foldl
foldlM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldlM = foldM

-- | Monadic version of foldl that discards its result
foldlM_ :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldlM_ = foldM_

-- | Monadic version of foldr
foldrM :: (Monad m) => (b -> a -> m a) -> a -> [b] -> m a
foldrM _ z []     = return z
foldrM k z (x:xs) = do { r <- foldrM k z xs; k x r }
