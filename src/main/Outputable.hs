--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module defines classes and functions for pretty-printing
--
--------------------------------------------------------------------------------

module Outputable (
    printOutput,
    printErrs, printWarns,
    progName,
    panic
  ) where

#include "versions.h"

import Bag
import ErrUtils

import System.IO (hPutStrLn, stderr)


-- -------------------------------------------------------------------
-- Print Messages

printOutput :: Messages -> IO ()
printOutput (warns, errs) = printMsgBag output
    where output = warns `unionBags` errs

printErrs :: Messages -> IO ()
printErrs = printMsgBag . snd

printWarns :: Messages -> IO ()
printWarns = printMsgBag . fst

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
