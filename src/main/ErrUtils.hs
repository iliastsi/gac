--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Utilities for error reporting
--
--------------------------------------------------------------------------------

module ErrUtils (
    Message, mkLocMessage,
    msgSpan, msgContext, msgExtraInfo,

    MsgCode,

    ErrMsg, WarnMsg,
    ErrorMessages, WarningMessages,
    Messages, errorsFound, emptyMessages,
    mkErrMsg, mkWarnMsg,
  ) where

import Bag
import SrcLoc


-- -------------------------------------------------------------------
-- Basic error codes

data MsgCode
  = UnknownErr

-- -------------------------------------------------------------------
-- Collecting up messages for later ordering and printing

data Message = Msg {
    msgSpan         :: SrcSpan,
    msgContext      :: MsgCode,
    msgExtraInfo    :: String
    }

mkLocMessage :: SrcSpan -> MsgCode -> String -> Message
mkLocMessage msgSpan msgContext msgExtraInfo =
    Msg msgSpan msgContext msgExtraInfo

-- An error message
type ErrMsg  = Message

mkErrMsg :: SrcSpan -> MsgCode -> String -> ErrMsg
mkErrMsg = mkLocMessage

-- A warning message
type WarnMsg = Message

mkWarnMsg :: SrcSpan -> MsgCode -> String -> WarnMsg
mkWarnMsg = mkLocMessage

type Messages = (Bag WarnMsg, Bag ErrMsg)

type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

errorsFound :: Messages -> Bool
errorsFound (warns, errs) = not (isEmptyBag errs)
