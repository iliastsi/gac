--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module defines classes and functions for pretty-printing
--
--------------------------------------------------------------------------------

module Outputable (
    printOutput,
    printErrs, printWarns,
    printMessages,
    progName,
    panic
  ) where

#include "versions.h"

import Bag
import ErrUtils

import System.IO (hPutStrLn, stderr)


-- -------------------------------------------------------------------
-- Print Output

printOutput :: String -> IO ()
printOutput = putStrLn

printErrs :: String -> IO ()
printErrs = hPutStrLn stderr

printWarns :: String -> IO ()
printWarns = hPutStrLn stderr


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
