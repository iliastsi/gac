--------------------------------------------------------------------------------
--
-- Prelude Alan Library
--
-- (c) Tsitsimpis Ilias, 2011-2012
--
--------------------------------------------------------------------------------

module Main (main) where

import Data.Word
import Data.Int
import LLVM.Core


prelude :: CodeGenModule ()
prelude = do
    -- declare all our functions
    printf <- newNamedFunction ExternalLinkage "printf" ::
        TFunction (Ptr Word8 -> VarArgs Word32)
    scanf <- newNamedFunction ExternalLinkage "scanf"  ::
        TFunction (Ptr Word8 -> VarArgs Word32)
    writeInteger <- newNamedFunction ExternalLinkage "writeInteger" ::
        TFunction (Int32 -> IO ())
    writeByte <- newNamedFunction ExternalLinkage "writeByte" ::
        TFunction (Word8 -> IO ())
    writeChar <- newNamedFunction ExternalLinkage "writeChar" ::
        TFunction (Word8 -> IO ())
    writeString <- newNamedFunction ExternalLinkage "writeString" ::
        TFunction (Ptr Word8 -> IO ())
    readInteger <- newNamedFunction ExternalLinkage "readInteger" ::
        TFunction (IO Int32)
    readByte <- newNamedFunction ExternalLinkage "readByte" ::
        TFunction (IO Word8)
    readChar <- newNamedFunction ExternalLinkage "readChar" ::
        TFunction (IO Word8)
    readString <- newNamedFunction ExternalLinkage "readString" ::
        TFunction (Int32 -> Ptr Word8 -> IO ())
    extend <- newNamedFunction ExternalLinkage "extend" ::
        TFunction (Word8 -> IO Int32)
    shrink <- newNamedFunction ExternalLinkage "shrink" ::
        TFunction (Int32 -> IO Word8)
    strlen <- newNamedFunction ExternalLinkage "strlen" ::
        TFunction (Ptr Word8 -> IO Int32)
    strcmp <- newNamedFunction ExternalLinkage "strcmp" ::
        TFunction (Ptr Word8 -> Ptr Word8 -> IO Int32)
    strcpy <- newNamedFunction ExternalLinkage "strcpy" ::
        TFunction (Ptr Word8 -> Ptr Word8 -> IO ())
    strcat <- newNamedFunction ExternalLinkage "strcat" ::
        TFunction (Ptr Word8 -> Ptr Word8 -> IO ())

    -- -----------------------
    -- cast printf/scanf
    let print_d = castVarArgs printf :: Function (Ptr Word8 -> Int32 -> IO Word32)

    -- -----------------------
    -- writeInteger
    withStringNul "%d" (\d ->
        defineFunction writeInteger $ \i -> do
            t1 <- getElementPtr d (0::Word32, (0::Word32, ()))
            _ <- call print_d t1 i
            ret ()
        )

    return ()


main :: IO ()
main = do
    mPrelude <- newModule
    defineModule mPrelude prelude
    writeBitcodeToFile "prelude.bc" mPrelude