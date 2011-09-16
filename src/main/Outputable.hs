--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- This module defines classes and functions for pretty-printing
--
--------------------------------------------------------------------------------

module Outputable (
    printOutput,
    printErrs, printWarns,
    printPlain
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
    mapM_ (\msg -> putStrLn $ (show msg) ++ "\n") (sortMessages (bagToList msgBag))


-- -------------------------------------------------------------------
-- Print SrcSpan'ed string

printPlain :: SrcSpan -> String -> IO ()
printPlain mspan msg = do
    let file = srcSpanFile mspan
        line = show $ srcSpanStartLine mspan
        col  = show $ srcSpanStartCol  mspan
    putStrLn (file ++ ":" ++ line ++ ":" ++ col ++ ": " ++ msg)
