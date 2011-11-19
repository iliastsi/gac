--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Utilities for error reporting
--
--------------------------------------------------------------------------------

module ErrUtils (
    Message, mkLocMessage,
    msgSpan, msgContext, msgSeverity, msgExtraInfo,

    MsgCode(..),
    Severity(..),

    ErrMsg, WarnMsg,
    ErrorMessages, WarningMessages,
    Messages, errorsFound, warnsFound,
    emptyMessages, unionMessages,
    mkErrMsg, mkWarnMsg,

    sortMessages,

    addError, addWarning
  ) where

import Bag
import SrcLoc
import Util


-- -------------------------------------------------------------------
-- Basic error codes
-- Don't forget to add the error message into Show instance

data MsgCode
  = ParseError String
  | TypeError  String
  | ScopeError String
  | UnreachError        -- unreachable code
  | RedefError String   -- function/variable redefinition
  | NoRetError String   -- missing return statement
  | UnknownError

data Severity
  = SevInfo
  | SevOutput
  | SevWarning
  | SevError
  | SevFatal

-- -------------------------------------------------------------------
-- Collecting up messages for later ordering and printing

data Message = Msg {
    msgSeverity     :: Severity,
    msgSpan         :: SrcSpan,
    msgContext      :: MsgCode,
    msgExtraInfo    :: String
    }

mkLocMessage :: Severity -> SrcSpan -> MsgCode -> String -> Message
mkLocMessage msgSev msgSpan msgContext msgExtraInfo =
    Msg msgSev msgSpan msgContext msgExtraInfo

-- An error message
type ErrMsg  = Message

mkErrMsg :: SrcSpan -> MsgCode -> String -> ErrMsg
mkErrMsg = mkLocMessage SevError

-- A warning message
type WarnMsg = Message

mkWarnMsg :: SrcSpan -> MsgCode -> String -> WarnMsg
mkWarnMsg = mkLocMessage SevWarning

type Messages = (Bag WarnMsg, Bag ErrMsg)

type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

errorsFound :: Messages -> Bool
errorsFound (warns, errs) = not (isEmptyBag errs)

warnsFound :: Messages -> Bool
warnsFound (warns, errs) = not (isEmptyBag warns)

unionMessages :: Messages -> Messages -> Messages
unionMessages (w1, e1) (w2, e2) =
    (w1 `unionBags` w2, e1 `unionBags` e2)

-- -------------------------------------------------------------------
-- Sort a list of messages by descending SrcSpan order

sortMessages :: [Message] -> [Message]
sortMessages = sortLe (\Msg{msgSpan=s1} Msg{msgSpan=s2} -> s1<=s2)


-- -------------------------------------------------------------------
-- Add new messages to the bag

addError :: ErrMsg -> Messages -> Messages
addError err (warns, errs) =
    let errs' = errs `snocBag` err
    in
    errs' `seq` (warns, errs')

addWarning :: WarnMsg -> Messages -> Messages
addWarning warn (warns, errs) =
    let warns' = warns `snocBag` warn
    in
    warns' `seq` (warns', errs)

-- -------------------------------------------------------------------
-- Instance declartions

instance Show Severity where
    show SevInfo    = "Info"
    show SevWarning = "Warning"
    show SevError   = "Error"
    show SevFatal   = "Fatal Error"

instance Show MsgCode where
    show (ParseError "")      = "Parse error at end of file"
    show (ParseError buf)     = "Parse error on input `" ++ buf ++ "'"
    show (ScopeError ide)     = "Not in scope `" ++ ide ++ "'"
    show (TypeError expr)      = "Type error at `" ++ expr ++ "'"
    show UnreachError         = "Unreachable code"
    show (RedefError ide)     = "Conflicting definitions for `" ++ ide ++ "'"
    show (NoRetError ide)     = "Control reaches end of non-proc function `" ++ ide ++ "'"
    show UnknownError         = "Unknown Error :@"
