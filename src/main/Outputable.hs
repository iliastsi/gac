--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module defines classes and functions for pretty-printing
--
--------------------------------------------------------------------------------

module Outputable (
    printOutput,
    printErrs, printWarns
  ) where

import SrcLoc
import Bag
import ErrUtils


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
    mapM_ (\msg -> print $ (show msg) ++ "\n") (sortMessages (bagToList msgBag))
