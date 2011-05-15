--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module SrcLoc (
    Located(..), SrcLoc(..), startSrcLoc,
    getSrcOffset, getSrcLine, getSrcColumn,
    incSrcLine, incSrcColumn, incSrcTab,
    srcCarRet
  ) where

data Located e = L SrcLoc e
    deriving Show

data SrcLoc = SrcLoc {
    offset  :: !Int,    -- absolute character offset
    line    :: !Int,    -- line number
    column  :: !Int     -- column number
  } deriving (Eq, Show)

startSrcLoc :: SrcLoc
startSrcLoc = SrcLoc 1 1 1

getSrcOffset :: SrcLoc -> Int
getSrcOffset SrcLoc{offset=offset'} = offset'

getSrcLine :: SrcLoc -> Int
getSrcLine SrcLoc{line=line'} = line'

getSrcColumn :: SrcLoc -> Int
getSrcColumn SrcLoc{column=column'} = column'

incSrcLine :: SrcLoc -> SrcLoc
incSrcLine loc@SrcLoc{offset=offset', line=line'} =
            loc{offset=offset'+1, line=line'+1, column=1}

incSrcColumn :: SrcLoc -> SrcLoc
incSrcColumn loc@SrcLoc{offset=offset', column=column'} =
            loc{offset=offset'+1, column=column'+1}

incSrcTab :: SrcLoc -> SrcLoc
incSrcTab loc@SrcLoc{offset=offset', column=column'} =
            loc{offset=offset'+1, column=column''}
            where column'' = column' + 5 - (column' `mod` 4)

srcCarRet :: SrcLoc -> SrcLoc
srcCarRet loc@SrcLoc{offset=offset', column=column'} =
            loc{offset=offset'-column'+1, column=1}
