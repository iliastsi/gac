--------------------------------------------------------------------------------
--
-- Prelude Alan Library
--
-- (c) Tsitsimpis Ilias, 2011-2012
--
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
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
    getchar <- newNamedFunction ExternalLinkage "getchar" ::
        TFunction (IO Int32)
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
    _ <- newNamedFunction ExternalLinkage "strlen" ::
        TFunction (Ptr Word8 -> IO Int32)
    _ <- newNamedFunction ExternalLinkage "strcmp" ::
        TFunction (Ptr Word8 -> Ptr Word8 -> IO Int32)
    _ <- newNamedFunction ExternalLinkage "strcpy" ::
        TFunction (Ptr Word8 -> Ptr Word8 -> IO ())
    _ <- newNamedFunction ExternalLinkage "strcat" ::
        TFunction (Ptr Word8 -> Ptr Word8 -> IO ())

    -- -----------------------
    -- cast printf/scanf
    let print_d = castVarArgs printf :: Function (Ptr Word8 -> Int32 -> IO Word32)
        print_b = castVarArgs printf :: Function (Ptr Word8 -> Word8 -> IO Word32)
        print_s = castVarArgs printf :: Function (Ptr Word8 -> IO Word32)
        read_d = castVarArgs scanf :: Function (Ptr Word8 -> Ptr Int32 -> IO Word32)
        read_c = castVarArgs scanf :: Function (Ptr Word8 -> Ptr Word8 -> IO Word32)

    -- -----------------------
    -- writeInteger
    withStringNul "%d" (\fmt ->
        defineFunction writeInteger $ \i -> do
            t1 <- getElementPtr fmt (0::Word32, (0::Word32, ()))
            _ <- call print_d t1 i
            ret ()
        )

    -- -----------------------
    -- writeByte
    withStringNul "%d" (\fmt ->
        defineFunction writeByte $ \b -> do
            t1 <- getElementPtr fmt (0::Word32, (0::Word32, ()))
            _ <- call print_b t1 b
            ret ()
        )

    -- -----------------------
    -- writeChar
    withStringNul "%c" (\fmt ->
        defineFunction writeChar $ \c -> do
            t1 <- getElementPtr fmt (0::Word32, (0::Word32, ()))
            _ <- call print_b t1 c
            ret ()
        )

    -- -----------------------
    -- writeString
    defineFunction writeString $ \s -> do
        _ <- call print_s s
        ret ()

    -- -----------------------
    -- readInteger
    withStringNul "%d" (\fmt ->
        defineFunction readInteger $ do
            t1 <- alloca
            t2 <- getElementPtr fmt (0::Word32, (0::Word32, ()))
            _ <- call read_d t2 t1
            t3 <- load t1
            ret t3
        )

    -- -----------------------
    -- readByte
    defineFunction readByte $ do
        t1 <- call readInteger
        (t2 :: Value Word8) <- trunc t1
        ret t2

    -- -----------------------
    -- readChar
    withStringNul "%c" (\fmt ->
        defineFunction readChar $ do
            t1 <- alloca
            t2 <- getElementPtr fmt (0::Word32, (0::Word32, ()))
            _ <- call read_c t2 t1
            t3 <- load t1
            ret t3
        )

    -- -----------------------
    -- readString
    defineFunction readString $ \n s -> do
        top <- getCurrentBasicBlock
        loop <- newBasicBlock
        body <- newBasicBlock
        bb1 <- newBasicBlock
        bb2 <- newBasicBlock
        exit <- newBasicBlock
        n' <- sub n (valueOf (1::Int32))
        br loop
        -- loop
        defineBasicBlock loop
        i <- phi [(n', top)] -- i starts as (n-1), when entered from top bb
        p <- phi [(s, top)] -- p starts as s, when entered from top bb
        t1 <- cmp CmpGT i (valueOf (0::Int32))
        condBr t1 body exit
        -- body
        defineBasicBlock body
        t2 <- call getchar
        t3 <- cmp CmpEQ t2 (valueOf (10::Int32))
        condBr t3 exit bb1
        -- bb1
        defineBasicBlock bb1
        t4 <- cmp CmpEQ t2 (valueOf (-1::Int32))
        condBr t4 exit bb2
        -- bb2
        defineBasicBlock bb2
        (t5 :: Value Word8) <- trunc t2
        store t5 p
        i' <- sub i (valueOf (1::Int32))
        p' <- getElementPtr p (1::Word32, ())
        addPhiInputs i [(i', bb2)]
        addPhiInputs p [(p', bb2)]
        br loop
        -- exit
        defineBasicBlock exit
        store (valueOf (0::Word8)) p
        ret ()

    -- -----------------------
    -- extend
    defineFunction extend $ \w -> do
        (t1 :: Value Int32) <- zext w
        ret t1

    -- -----------------------
    -- shrink
    defineFunction shrink $ \i -> do
        (t1 :: Value Word8) <- trunc i
        ret t1

    -- -----------------------
    -- string functions are
    -- the same with libc
    return ()


main :: IO ()
main = do
    mPrelude <- newModule
    defineModule mPrelude prelude
    writeBitcodeToFile "prelude.bc" mPrelude
