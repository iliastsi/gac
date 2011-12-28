--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module contains types that relate to the position of things
-- in source files, and allow tagging of those things with locations
--
--------------------------------------------------------------------------------

module SrcLoc (
    -- * SrcLoc
    SrcLoc,                 -- Abstract

    -- ** Constructing SrcLoc
    mkSrcLoc, mkGeneralSrcLoc,

    noSrcLoc,               -- "I'm sorry, I haven't a clue"
    generatedSrcLoc,        -- Code generated within compiler
    interactiveSrcLoc,      -- Code from an interactive session

    advanceSrcLoc,

    -- ** Unsafely deconstructing SrcLoc
    -- These are dubious exports, because they crash on some inputs
    srcLocFile,             -- return the file name part
    srcLocLine,             -- return the line part
    srcLocCol,              -- return the column part

    -- ** Predicates on SrcLoc
    isGoodSrcLoc,

    -- ** SrcLoc to String
    showSrcLoc,

    -- * SrcSpan
    SrcSpan,                -- Abstract

    -- ** Constructing SrcSpan
    mkGeneralSrcSpan, mkSrcSpan,
    noSrcSpan,
    wiredInSrcSpan,         -- Something wired into the compiler
    srcLocSpan,
    combineSrcSpans,

    -- ** Deconstructing SrcSpan
    srcSpanStart, srcSpanEnd,
    srcSpanFileName_maybe,

    -- ** Unsafely deconstructing SrcSpan
    -- These are dubious exports, because they crash on some inputs
    srcSpanFile,
    srcSpanStartLine, srcSpanEndLine,
    srcSpanStartCol, srcSpanEndCol,

    -- ** Predicates on SrcSpan
    isGoodSrcSpan, isOneLineSpan,

    -- ** SrcSpan to String
    showSrcSpan,

    -- * Located
    Located(..),

    -- ** Constructing Located
    noLoc, mkGeneralLocated,

    -- ** Deconstructing Located
    getLoc, unLoc,

    -- * Combining and comparing Located values
    eqLocated, cmpLocated, combineLocs, addCLoc,
  ) where

import Util
import {-# Source #-} Outputable(panic)

-- Uncomment this if use advanceSrcLoc with '\t'
--import Data.Bits


-- -------------------------------------------------------------------
-- Source-location information
data SrcLoc
  = SrcLoc  String          -- A precise location (file name)
            !Int                -- line number, begins at 1
            !Int                -- column number, begins at 1
  | UnhelpfulLoc String     -- Just a general indication


-- -------------------------------------------------------------------
-- Access functions
mkSrcLoc :: String -> Int -> Int -> SrcLoc
mkSrcLoc x line col = SrcLoc x line col

-- | Built-in "bad" 'SrcLoc' values for particular locations
noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc          = UnhelpfulLoc "<no location info>"
generatedSrcLoc   = UnhelpfulLoc "<compiler-generated code>"
interactiveSrcLoc = UnhelpfulLoc "<interactive session>"

-- | Creates a "bad" 'SrcLoc' that has no detailed information about its location
mkGeneralSrcLoc :: String -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc

-- | "Good" 'SrcLoc's have precise information about their location
isGoodSrcLoc :: SrcLoc -> Bool
isGoodSrcLoc (SrcLoc _ _ _) = True
isGoodSrcLoc _other         = False

-- | Gives the filename of the 'SrcLoc' if it is available, otherwise returns a dummy value
srcLocFile :: SrcLoc -> String
srcLocFile (SrcLoc fname _ _) = fname
srcLocFile _other             = "<unknown file"

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocLine :: SrcLoc -> Int
srcLocLine (SrcLoc _ l _) = l
srcLocLine (UnhelpfulLoc _) = panic "SrcLoc.srcLocLine can't handle `UnhelpfulLoc'"

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocCol :: SrcLoc -> Int
srcLocCol (SrcLoc _ _ c) = c
srcLocCol (UnhelpfulLoc _) = panic "SrcLoc.srcLocCol can't handle `UnhelpfulLoc'"

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l _) '\r' = SrcLoc f  l 1
--advanceSrcLoc (SrcLoc f l c) '\t' =
--    SrcLoc f  l (((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)
advanceSrcLoc loc            _    = loc -- Better than nothing

-- | Convert SrcLoc to String
showSrcLoc :: SrcLoc -> String
showSrcLoc (UnhelpfulLoc str) = str ++ ":"
showSrcLoc (SrcLoc name line col)   =
    name ++ ":" ++ show line ++ ":" ++ show col ++ ":"


-- -------------------------------------------------------------------
-- Instance declarations for various names
instance Eq SrcLoc where
    loc1 == loc2 = case loc1 `cmpSrcLoc` loc2 of
                        EQ -> True
                        _  -> False

instance Ord SrcLoc where
    compare = cmpSrcLoc

cmpSrcLoc :: SrcLoc -> SrcLoc -> Ordering
cmpSrcLoc (UnhelpfulLoc s1) (UnhelpfulLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulLoc _)  _other            = LT

cmpSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)
  = (s1 `compare` s2) `thenCmp` (l1 `compare` l2) `thenCmp` (c1 `compare` c2)
cmpSrcLoc (SrcLoc _ _ _) _other = GT


-- -------------------------------------------------------------------
-- Source Spans

{- |
A SrcSpan delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data SrcSpan
  = SrcSpanOneLine
      { srcSpanFile     :: !String,
        srcSpanLine     :: !Int,
        srcSpanSCol     :: !Int,
        srcSpanECol     :: !Int
      }

  | SrcSpanMultiLine
      { srcSpanFile     :: !String,
        srcSpanSLine    :: !Int,
        srcSpanSCol     :: !Int,
        srcSpanELine    :: !Int,
        srcSpanECol     :: !Int
      }

  | SrcSpanPoint
      { srcSpanFile     :: !String,
        srcSpanLine     :: !Int,
        srcSpanCol      :: !Int
      }

  | UnhelpfulSpan !String   -- Just a general indication
                            -- also used to indicate an empty span
  deriving Eq

-- | Built-in "bad" 'SrcSpan's for common sources of location uncertainty
noSrcSpan, wiredInSrcSpan :: SrcSpan
noSrcSpan      = UnhelpfulSpan "<no location info>"
wiredInSrcSpan = UnhelpfulSpan "<wired into compilers>"

-- | Create a "bad" 'SrcSpan' that has not location information
mkGeneralSrcSpan :: String -> SrcSpan
mkGeneralSrcSpan = UnhelpfulSpan

-- | Create a 'SrcSpan' corresponding to a single point
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (SrcLoc file line col) = SrcSpanPoint file line col

-- | Create a 'SrcSpan' between two points in a file
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan loc1 loc2
  | line1 == line2 = if col1 == col2
                        then SrcSpanPoint file line1 col1
                        else SrcSpanOneLine file line1 col1 col2
  | otherwise      = SrcSpanMultiLine file line1 col1 line2 col2
  where
    line1 = srcLocLine loc1
    line2 = srcLocLine loc2
    col1 = srcLocCol loc1
    col2 = srcLocCol loc2
    file = srcLocFile loc1

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans l (UnhelpfulSpan _) = l
combineSrcSpans start end
  = case line1 `compare` line2 of
         EQ -> case col1 `compare` col2 of
                    EQ -> SrcSpanPoint file line1 col1
                    LT -> SrcSpanOneLine file line1 col1 col2
                    GT -> SrcSpanOneLine file line1 col2 col1
         LT -> SrcSpanMultiLine file line1 col1 line2 col2
         GT -> SrcSpanMultiLine file line2 col2 line1 col1
  where
    line1 = srcSpanStartLine start
    col1  = srcSpanStartCol start
    line2 = srcSpanEndLine end
    col2  = srcSpanEndCol end
    file  = srcSpanFile start


-- -------------------------------------------------------------------
-- Predicates

-- | Test if a 'SrcSpan' is "good", i.e. has precise location information
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan SrcSpanOneLine{}   = True
isGoodSrcSpan SrcSpanMultiLine{} = True
isGoodSrcSpan SrcSpanPoint{}     = True
isGoodSrcSpan _                  = False

isOneLineSpan :: SrcSpan -> Bool
-- ^ True if the span is known to straddle only one line.
-- For "bad" 'SrcSpan', it returns False
isOneLineSpan s
  | isGoodSrcSpan s = srcSpanStartLine s == srcSpanEndLine s
  | otherwise       = False


-- -------------------------------------------------------------------
-- Unsafe access functions

-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanStartLine :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanEndLine :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanStartCol :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanEndCol :: SrcSpan -> Int

srcSpanStartLine SrcSpanOneLine{ srcSpanLine=l }    = l
srcSpanStartLine SrcSpanMultiLine{ srcSpanSLine=l } = l
srcSpanStartLine SrcSpanPoint{ srcSpanLine=l }      = l
srcSpanStartLine _ = panic "SrcLoc.srcSpanStartLine got unexpected input"

srcSpanEndLine SrcSpanOneLine{ srcSpanLine=l }    = l
srcSpanEndLine SrcSpanMultiLine{ srcSpanELine=l } = l
srcSpanEndLine SrcSpanPoint{ srcSpanLine=l }      = l
srcSpanEndLine _ = panic "SrcLoc.srcSpanEndLine got unexpected input"

srcSpanStartCol SrcSpanOneLine{ srcSpanSCol=l }   = l
srcSpanStartCol SrcSpanMultiLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanPoint{ srcSpanCol=l }      = l
srcSpanStartCol _ = panic "SrcLoc.srcSpanStartCol got unexpected input"

srcSpanEndCol SrcSpanOneLine{ srcSpanECol=c }   = c
srcSpanEndCol SrcSpanMultiLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanPoint{ srcSpanCol=c }      = c
srcSpanEndCol _ = panic "SrcLoc.srcSpanEndCol got unexpected input"


-- -------------------------------------------------------------------
-- Access functions

-- | Returns the location at the start of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanStart :: SrcSpan -> SrcLoc
-- | Returns the location at the end of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanEnd :: SrcSpan -> SrcLoc

srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart s = mkSrcLoc (srcSpanFile s)
                          (srcSpanStartLine s)
                          (srcSpanStartCol s)

srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd s =
  mkSrcLoc (srcSpanFile s)
           (srcSpanEndLine s)
           (srcSpanEndCol s)

-- | Obtains the filename for a 'SrcSpan' if it is "good"
srcSpanFileName_maybe :: SrcSpan -> Maybe String
srcSpanFileName_maybe (SrcSpanOneLine { srcSpanFile = nm })   = Just nm
srcSpanFileName_maybe (SrcSpanMultiLine { srcSpanFile = nm }) = Just nm
srcSpanFileName_maybe (SrcSpanPoint { srcSpanFile = nm})      = Just nm
srcSpanFileName_maybe _                                       = Nothing

-- | Convert SrcSpan to String
showSrcSpan :: SrcSpan -> String
showSrcSpan (UnhelpfulSpan str) = str ++ ":"
showSrcSpan (SrcSpanOneLine name line scol ecol) =
    name ++ ":" ++ show line ++ ":" ++ show scol ++
        if (ecol - scol) <= 1 then "" else "-" ++ show (ecol-1) ++ ":"
showSrcSpan (SrcSpanMultiLine name sline scol eline ecol) =
    let ecol' = if ecol == 0 then ecol else (ecol-1) in
    name ++ ":" ++ show sline ++ "," ++ show (scol-1) ++ "-" ++
        show eline ++ "," ++ show ecol' ++ ":"
showSrcSpan (SrcSpanPoint name line col) =
    name ++ ":" ++ show line ++ ":" ++ show col ++ ":"



-- -------------------------------------------------------------------
-- Instances

-- We want to order SrcSpans first by the start point, then by the end point
instance Ord SrcSpan where
    a `compare` b =
        (srcSpanStart a `compare` srcSpanStart b) `thenCmp`
        (srcSpanEnd   a `compare` srcSpanEnd   b)

instance Show SrcSpan where
    show (UnhelpfulSpan str) = str ++ ":"
    show other_span          =
        let file = srcSpanFile other_span
            line = show $ srcSpanStartLine other_span
            col  = show $ srcSpanStartCol  other_span
        in
        file ++ ":" ++ line ++ ":" ++ col ++ ":"


-- -------------------------------------------------------------------
-- Attaching SrcSpans to things

-- | We attach SrcSpans to lots of things, so let's have a datatype for it
data Located e = L SrcSpan e
    deriving (Eq, Ord, Show)

unLoc :: Located e -> e
unLoc (L _ e) = e

getLoc :: Located e -> SrcSpan
getLoc (L l _) = l

noLoc :: e -> Located e
noLoc e = L noSrcSpan e

mkGeneralLocated :: String -> e -> Located e
mkGeneralLocated s e = L (mkGeneralSrcSpan s) e

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs a b = combineSrcSpans (getLoc a) (getLoc b)

-- | Combine locations from two 'Located' things and add them to a third thing
addCLoc :: Located a -> Located b -> c -> Located c
addCLoc a b c = L (combineSrcSpans (getLoc a) (getLoc b)) c

-- not clear whether to add a general Eq instance, but this is useful sometimes:

-- | Tests whether the two located things are equal
eqLocated :: Eq a => Located a -> Located a -> Bool
eqLocated a b = unLoc a == unLoc b

-- not clear whether to add a general Ord instance, but this is useful sometimes:

-- | Tests the ordering of the two located thing
cmpLocated :: Ord a => Located a -> Located a -> Ordering
cmpLocated a b = unLoc a `compare` unLoc b

instance Functor Located where
    fmap f (L l e) = L l (f e)
