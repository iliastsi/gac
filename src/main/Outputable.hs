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
import DynFlags

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

printMessages :: DynFlags -> Messages -> IO ()
printMessages dflags (warns, errs) = printMsgBag dflags output
    where output = warns `unionBags` errs

printMsgBag :: DynFlags -> Bag Message -> IO ()
printMsgBag dflags msgBag =
    mapM_ (\msg -> hPutStrLn stderr $ (showMsg dflags) ++ "\n") (sortMessages (bagToList msgBag))

showMsg :: DynFlags -> Messages -> String
showMsg dflags Msg{msgSeverity=sev,msgSpan=mspan,msgContext=code,msgExtraInfo=extra} =
    let extra' = if null extra then "" else "\n\t" ++ extra
        loc    = if dopt Opt_ErrorSpans dflags then showSpan mspan else showLoc (srcSpanStart mspan)
    in loc ++ " " ++ show sev ++ ": " ++ show code ++ extra'

showLoc :: SrcLoc -> String
showLoc UnhelpfulLoc str = str ++ ":"
showLoc (SrcLoc name line col)   =
    name ++ ":" ++ show line ++ ":" ++ show col ++ ":"

showSpan :: SrcSpan -> String
showSpan UnhelpfulSpan str = str ++ ":"
showSpan (SrcSpanOneLine name line scol ecol) =
    name ++ ":" ++ show line ++ ":" ++ show scol ++ "-" ++ show ecol ++ ":"
showSpan (SrcSpanMultiLine name sline scol eline ecol) =
    name ++ ":" ++ show sline ++ "," show scol "-" ++
        show eline ++ "," show ecol ++ ":"


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
