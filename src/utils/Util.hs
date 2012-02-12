--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- This module contains some highly random utility functions
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

    -- * List operations controlled by another list
    split,

    -- * Sorting
    sortLe, sortWith,

    -- * Comparisons
    isEqual, thenCmp,
    removeSpaces,

    -- * IO-ish utilities
    consIORef
  ) where

import Data.Char (isSpace)
import Data.IORef

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
-- List operations controlled by another list

split :: Char -> String -> [String]
split c s =
    case rest of
         []     -> [chunk]
         _:rest -> chunk : split c rest
    where (chunk, rest) = break (==c) s


-- -------------------------------------------------------------------
-- A mergesort from Carsten
--
-- Date: Mon, 3 May 93 20:45:23 +0200
-- From: Carsten Kehler Holst <kehler@cs.chalmers.se>
-- To: partain@dcs.gla.ac.uk
-- Subject: natural merge sort beats quick sort [ and it is prettier ]
--
-- Here is a piece of Haskell code that I'm rather fond of. See it as an
-- attempt to get rid of the ridiculous quick-sort routine. group is
-- quite useful by itself I think it was John's idea originally though I
-- believe the lazy version is due to me [surprisingly complicated].
-- gamma [used to be called] is called gamma because I got inspired by
-- the Gamma calculus. It is not very close to the calculus but does
-- behave less sequentially than both foldr and foldl. One could imagine
-- a version of gamma that took a unit element as well thereby avoiding
-- the problem with empty lists.
--
-- I've tried this code against
--
--    1) insertion sort - as provided by haskell
--    2) the normal implementation of quick sort
--    3) a deforested version of quick sort due to Jan Sparud
--    4) a super-optimized-quick-sort of Lennart's
--
-- If the list is partially sorted both merge sort and in particular
-- natural merge sort wins. If the list is random [ average length of
-- rising subsequences = approx 2 ] mergesort still wins and natural
-- merge sort is marginally beaten by Lennart's soqs. The space
-- consumption of merge sort is a bit worse than Lennart's quick sort
-- approx a factor of 2. And a lot worse if Sparud's bug-fix [see his
-- fpca article ] isn't used because of group.
--
-- have fun
-- Carsten

group :: (a -> a -> Bool) -> [a] -> [[a]]
-- Given a <= function, group finds maximal contiguous up-runs
-- or down-runs in the input list.
-- It's stable, in the sense that it never re-orders equal elements
--
-- Date: Mon, 12 Feb 1996 15:09:41 +0000
-- From: Andy Gill <andy@dcs.gla.ac.uk>
-- Here is a `better' definition of group.
group _ []      = []
group p (x:xs)  = group' xs x x (x :)
  where
    group' []     _     _     s = [s []]
    group' (x:xs) x_min x_max s
        |      x_max `p` x  = group' xs x_min x     (s . (x :))
        | not (x_min `p` x) = group' xs x     x_max ((x :) . s)
        | otherwise         = s [] : group' xs x x (x :)
        -- NB: the 'not' is essential for stablity
        --     x `p` x_min would reverse equal elements

generalMerge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
generalMerge _ xs [] = xs
generalMerge _ [] ys = ys
generalMerge p (x:xs) (y:ys)
    | x `p` y   = x : generalMerge p xs     (y:ys)
    | otherwise = y : generalMerge p (x:xs) ys

-- gamma is now called balancedFold

balancedFold :: (a -> a -> a) -> [a] -> a
balancedFold _ []  = error "can't reduce an empty list using balancedFold"
balancedFold _ [x] = x
balancedFold f l   = balancedFold f (balancedFold' f l)

balancedFold' :: (a -> a -> a) -> [a] -> [a]
balancedFold' f (x:y:xs) = f x y : balancedFold' f xs
balancedFold' _ xs = xs

generalNaturalMergeSort :: (a -> a -> Bool) -> [a] -> [a]
generalNaturalMergeSort _ [] = []
generalNaturalMergeSort p xs = (balancedFold (generalMerge p) . group p) xs

sortLe :: (a -> a -> Bool) -> [a] -> [a]
sortLe le = generalNaturalMergeSort le

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith get_key xs = sortLe le xs
    where
        x `le` y = get_key x < get_key y


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

removeSpaces :: String -> String
removeSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace


-- -------------------------------------------------------------------
-- IO-ish utilities

consIORef :: IORef [a] -> a -> IO ()
consIORef var x = do
    atomicModifyIORef var (\xs -> (x:xs,()))
