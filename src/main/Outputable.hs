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
    printMessages,
    progName,
    panic
  ) where

#include "versions.h"

import Bag
import ErrUtils
import SrcLoc

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


-- -------------------------------------------------------------------
-- Print Messages

printMessages :: Messages -> IO ()
printMessages (warns, errs) = printMsgBag output
    where output = warns `unionBags` errs

printMsgBag :: Bag Message -> IO ()
printMsgBag msgBag =
    mapM_ (\msg -> hPutStrLn stderr $ (show msg) ++ "\n") (sortMessages (bagToList msgBag))


-- -------------------------------------------------------------------
-- The name of the program

progName :: String
progName = PROG_NAME


-- -------------------------------------------------------------------
-- Internal Errors

panic :: String -> a
panic str = error ("My brain just exploded\n\t" ++ str ++ "\n")
