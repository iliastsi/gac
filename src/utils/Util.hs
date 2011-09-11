--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module contains some highly random utility functions
--
--------------------------------------------------------------------------------

module Util (
    -- * General list processing
    zipLazy,

    unzipWith,

    mapFst, mapSnd,
    mapAndUnzip,
    nOfThem, filterOut, partitionWith, splitEithers,

    lengthExceeds, lengthIs, lengthAtLeast,
    listLengthCmp, atLength, equalLength, compareLength,

    isSingleton, singleton,
    notNull,

    -- * Comparisons
    isEqual, thenCmp
  ) where


infixr 9 `thenCmp`

-- -------------------------------------------------------------------
-- General list processing

-- | 'zipLazy' is a kind of 'zip' that is lazy in the second list (observe the ~)
zipLazy :: [a] -> [b] -> [(a,b)]
zipLazy []      _       = []
-- We want to write this, but with GHC 6.4 we get a warning, so it
-- doesn't validate:
-- zipLazy (x:xs) ~(y:ys) = (x,y) : zipLazy xs ys
-- so we write this instead:
zipLazy (x:xs) zs = let y : ys = zs
                    in (x,y) : zipLazy xs ys

unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map (\ (a, b) -> f a b) pairs

mapFst :: (a->c) -> [(a,b)] -> [(c,b)]
mapSnd :: (b->c) -> [(a,b)] -> [(a,c)]

mapFst f xys = [(f x, y) | (x,y) <- xys]
mapSnd f xys = [(x, f y) | (x,y) <- xys]

mapAndUnzip :: (a -> (b,c)) -> [a] -> ([b], [c])
mapAndUnzip _ [] = ([], [])
mapAndUnzip f (x:xs) =
    let (r1, r2)   = f x
        (rs1, rs2) = mapAndUnzip f xs
    in
    (r1:rs1, r2:rs2)

nOfThem :: Int -> a -> [a]
nOfThem n thing = replicate n thing

filterOut :: (a->Bool) -> [a] -> [a]
-- ^ Like filter, only it reverses the sense of the test
filterOut _ [] = []
filterOut p (x:xs)
  | p x         = filterOut p xs
  | otherwise   = x : filterOut p xs

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
-- ^ Uses a function to determine which of two output lists an input element should join
partitionWith _ [] = ([], [])
partitionWith f (x:xs) =
    case f x of
         Left  b -> (b:bs, cs)
         Right c -> (bs, c:cs)
    where (bs, cs) = partitionWith f xs

splitEithers :: [Either a b] -> ([a], [b])
-- ^ Teases a list of 'Either's apart int two lists
splitEithers [] = ([], [])
splitEithers (e:es) =
    case e of
         Left x  -> (x:xs, ys)
         Right y -> (xs, y:ys)
    where (xs, ys) = splitEithers es

-- | @atLength atLen atEnd ls n@ unravels list @ls@ to position @n@. Precisely:
--
-- @
--  atLenfth atLenPred atEndPred ls n
--    | n < 0           = atEndPred n
--    | length ls < n   = atEndPred (n - length ls)
--    | otherwise       = atLendPred (drop n ls)
-- @
atLength :: ([a] -> b)
         -> (Int -> b)
         -> [a]
         -> Int
         -> b
atLength atLenPred atEndPred ls n
  | n < 0     = atEndPred n
  | otherwise = go n ls
  where
    go n [] = atEndPred n
    go 0 ls = atLenPred ls
    go n (_:xs) = go (n-1) xs

-- Some special cases of atLength:

lengthExceeds :: [a] -> Int -> Bool
-- ^ > (lengthExceeds xs n) = length xs > n)
lengthExceeds = atLength notNull (const False)

lengthAtLeast :: [a] -> Int -> Bool
lengthAtLeast = atLength notNull (== 0)

lengthIs :: [a] -> Int -> Bool
lengthIs = atLength null (== 0)

listLengthCmp :: [a] -> Int -> Ordering
listLengthCmp = atLength atLen atEnd
  where
    atEnd 0         = EQ
    atEnd x
      | x > 0       = LT -- not yet seen 'n' elts, so list length is < n.
      | otherwise   = GT

    atLen []        = EQ
    atLen _         = GT

equalLength :: [a] -> [b] -> Bool
equalLength []      []      = True
equalLength (_:xs)  (_:ys)  = equalLength xs ys
equalLength _       _       = False

compareLength :: [a] -> [b] -> Ordering
compareLength []        []      = EQ
compareLength (_:xs)    (_:ys)  = compareLength xs ys
compareLength []        _       = LT
compareLength _         []      = GT

------------------------------
singleton :: a -> [a]
singleton x = [x]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

notNull :: [a] -> Bool
notNull [] = False
notNull _  = True

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
