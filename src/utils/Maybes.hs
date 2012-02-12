--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Utilities related to Data.Maybe
--
--------------------------------------------------------------------------------

module Maybes (
    module Data.Maybe,

    MaybeErr(..), -- Instance of Monad
    failME, isSuccess,

    fmapM_maybe,
    orElse,
    mapCatMaybes,
    allMaybes,
    firstJust, firstJusts,
    expectJust,
    maybeToBool,

    MaybeT(..)
  ) where

import Data.Maybe

infix 4 `orElse`


-- -------------------------------------------------------------------
-- The @Maybe@ type

maybeToBool :: Maybe a -> Bool
maybeToBool Nothing  = False
maybeToBool (Just _) = True

-- | Collects a list of @Justs@ into a single @Just@, returning @Nothing@ if
-- there are any @Nothings@.
allMaybes :: [Maybe a] -> Maybe [a]
allMaybes [] = Just []
allMaybes (Nothing : _)  = Nothing
allMaybes (Just x  : ms) =
    case allMaybes ms of
         Nothing -> Nothing
         Just xs -> Just (x:xs)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing  b = b

-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: [Maybe a] -> Maybe a
firstJusts = foldr firstJust Nothing

expectJust :: String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust _   (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)

mapCatMaybes :: (a -> Maybe b) -> [a] -> [b]
mapCatMaybes _ [] = []
mapCatMaybes f (x:xs) =
    case f x of
         Just y  -> y : mapCatMaybes f xs
         Nothing -> mapCatMaybes f xs

orElse :: Maybe a -> a -> a
(Just x) `orElse` _ = x
Nothing  `orElse` y = y

fmapM_maybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
fmapM_maybe _ Nothing = return Nothing
fmapM_maybe f (Just x) = do
    x' <- f x
    return $ Just x'


-- -------------------------------------------------------------------
-- The @MaybeT@ monad transformer

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
    fmap f x = MaybeT $ fmap (fmap f) $ runMaybeT x

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
    fail _ = MaybeT $ return Nothing


-- -------------------------------------------------------------------
-- The @MaybeErr@ type

data MaybeErr err val = Succeeded val | Failed err

instance Monad (MaybeErr err) where
    return v = Succeeded v
    Succeeded v >>= k = k v
    Failed e   >>= _ = Failed e

isSuccess :: MaybeErr err val -> Bool
isSuccess (Succeeded {}) = True
isSuccess (Failed {})    = False

failME :: err -> MaybeErr err val
failME e = Failed e
