--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- This module defines classes and functions for pretty-printing
--
--------------------------------------------------------------------------------

module Outputable (
    printOutput,
    printErrs, printWarns,
    printLocErrs, printLocWarns,
    usageString,

    printMessages,

    printDumpedAst,

    progName,
    panic
  ) where

import Bag
import ErrUtils
import SrcLoc
import UnTypedAst (UAst, dumpedUAst)
import DynFlags

import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment


-- -------------------------------------------------------------------
-- Print Output

printOutput :: [String] -> IO ()
printOutput = mapM_ putStrLn

printErrs :: [String] -> IO ()
printErrs = mapM_ (hPutStrLn stderr)

printWarns :: [String] -> IO ()
printWarns = printErrs

printLocErrs :: [Located String] -> IO ()
printLocErrs =
    mapM_ (\(L loc msg) -> hPutStrLn stderr (showSrcSpan loc ++ " " ++ msg))

printLocWarns :: [Located String] -> IO ()
printLocWarns = printLocErrs

usageString :: String
usageString = "Usage: For basic information, try the `--help' option."


-- -------------------------------------------------------------------
-- Print Messages

printMessages :: DynFlags -> Messages -> IO ()
printMessages dflags (warns, errs) = printMsgBag dflags output
    where output = warns `unionBags` errs

printMsgBag :: DynFlags -> Bag Message -> IO ()
printMsgBag dflags msgBag =
    mapM_ (\msg -> hPutStrLn stderr $ (showMsg dflags msg) ++ "\n")
                        (sortMessages (bagToList msgBag))


-- -------------------------------------------------------------------
-- Print dumped Ast
printDumpedAst :: Bool -> String -> UAst -> IO ()
printDumpedAst to_file out_file uast = do
    if to_file
       then writeFile out_file $ dumpedUAst uast
       else do
           putStrLn "==================== Parser ===================="
           putStrLn (dumpedUAst uast)
           putStr "\n\n"


-- -------------------------------------------------------------------
-- The name of the program

{-# NOINLINE progName #-}
progName :: String
progName = unsafePerformIO (getProgName)


-- -------------------------------------------------------------------
-- Internal Errors

panic :: String -> a
panic str = error ("My brain just exploded\n\t" ++ str ++ "\n")
