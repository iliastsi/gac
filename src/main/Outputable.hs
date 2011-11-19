--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
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

#include "versions.h"

import Bag
import ErrUtils
import SrcLoc
import UnTypedAst (UAst, dumpedUAst)

import System.IO (hPutStrLn, stderr)


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
    mapM_ (\(L loc msg) -> hPutStrLn stderr (show loc ++ " " ++ msg))

printLocWarns :: [Located String] -> IO ()
printLocWarns = printLocErrs

printString :: String
printString = "Usage: For basic information, try the `--help' option."


-- -------------------------------------------------------------------
-- Print Messages

printMessages :: Messages -> IO ()
printMessages (warns, errs) = printMsgBag output
    where output = warns `unionBags` errs

printMsgBag :: Bag Message -> IO ()
printMsgBag msgBag =
    mapM_ (\msg -> hPutStrLn stderr $ (show msg) ++ "\n") (sortMessages (bagToList msgBag))


-- -------------------------------------------------------------------
-- Print dumped Ast
printDumpedAst :: UAst -> IO ()
printDumpedAst uast = do
    putStrLn "\n==================== Parser ===================="
    putStrLn (dumpedUAst uast)


-- -------------------------------------------------------------------
-- The name of the program

progName :: String
progName = PROG_NAME


-- -------------------------------------------------------------------
-- Internal Errors

panic :: String -> a
panic str = error ("My brain just exploded\n\t" ++ str ++ "\n")
