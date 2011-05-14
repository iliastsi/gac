module SrcLoc where

data Located e = L SrcLoc e
    deriving Show

data SrcLoc = SrcLoc {
    offset  :: Int,     -- absolute character offset
    line    :: Int,     -- line number
    column  :: Int      -- column number
  } deriving (Eq, Show)


getSrcOffset :: SrcLoc -> Int
getSrcOffset SrcLoc{offset=offset'} = offset'

getSrcLine :: SrcLoc -> Int
getSrcLine SrcLoc{line=line'} = line'

getSrcColumn :: SrcLoc -> Int
getSrcColumn SrcLoc{column=column'} = column'
