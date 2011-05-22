--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module SrcLoc (
    Located(..), SrcLoc(..), mkSrcLoc,
    getSrcLine, getSrcColumn, getSrcFile,
    incSrcLine, incSrcColumn, incSrcTab,
    srcCarRet, getLoc, unLoc
  ) where

data Located e = L SrcLoc e
    deriving Show

data SrcLoc = SrcLoc
    String              -- A precise location (file name)
    !Int                -- line number, begins at 1
    !Int                -- column number, begins at 1
  deriving (Eq, Show)

mkSrcLoc :: String -> Int -> Int -> SrcLoc
mkSrcLoc x line col = SrcLoc x line col

getSrcLine :: SrcLoc -> Int
getSrcLine (SrcLoc _ l _) = l

getSrcColumn :: SrcLoc -> Int
getSrcColumn (SrcLoc _ _ c) = c

getSrcFile :: SrcLoc -> String
getSrcFile (SrcLoc x _ _) = x

incSrcLine :: SrcLoc -> SrcLoc
incSrcLine (SrcLoc x l _) = (SrcLoc x (l+1) 1)

incSrcColumn :: SrcLoc -> SrcLoc
incSrcColumn (SrcLoc x l c) = (SrcLoc x l (c+1))

incSrcTab :: SrcLoc -> SrcLoc
incSrcTab = incSrcColumn
--incSrcTab (SrcLoc x l c) = (SrcLoc x l c')
--            where c' = c + 5 - (c `mod` 4)

srcCarRet :: SrcLoc -> SrcLoc   -- carriage return '\r'
srcCarRet (SrcLoc x l _) = (SrcLoc x l 1)

getLoc :: Located a -> SrcLoc
getLoc (L loc _) = loc

unLoc :: Located a -> a
unLoc (L _ x) = x
