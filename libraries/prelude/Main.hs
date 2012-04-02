--------------------------------------------------------------------------------
--
-- Prelude Alan Library
--
-- (c) Tsitsimpis Ilias, 2011-2012
--
--------------------------------------------------------------------------------

module Main (main) where

import Data.Word
import LLVM.Core


prelude :: CodeGenModule ()
prelude = do
    -- declare all our functions
    printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Word32)
--    scanf  <- newNamedFunction ExternalLinkage "scanf"  :: TFunction (Ptr Word8 -> VarArgs Word32)
    return ()


main :: IO ()
main = do
    mPrelude <- newModule
    defineModule mPrelude prelude
    writeBitcodeToFile "prelude.bc" mPrelude
