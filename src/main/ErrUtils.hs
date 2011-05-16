--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module ErrUtils (
    Message, Messages, emptyMessages,
    mkWarnMsg, mkErrMsg, snocBag,
    mkLocWarnMsg, mkLocErrMsg
  ) where

import SrcLoc

type Bag a = [a]

type Message = String
type WarnMsg = Message
type ErrMsg = Message

type Messages = (Bag WarnMsg, Bag ErrMsg)

emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

emptyBag :: Bag a
emptyBag = []

mkWarnMsg :: String -> WarnMsg
mkWarnMsg msg = mkMsg ("Warning: " ++ msg)

mkErrMsg :: String -> ErrMsg
mkErrMsg msg = mkMsg ("Error: " ++ msg)

mkMsg :: String -> Message
mkMsg msg = msg

snocBag :: Bag Message -> Message -> Bag Message
snocBag bag msg = bag ++ [msg]

mkLocWarnMsg :: SrcLoc -> String -> WarnMsg
mkLocWarnMsg pos msg = mkLocMsg pos ("Warning: " ++ msg)

mkLocErrMsg :: SrcLoc -> String -> ErrMsg
mkLocErrMsg pos msg = mkLocMsg pos ("Error: " ++ msg)

mkLocMsg :: SrcLoc -> String -> Message
mkLocMsg loc msg =
    let lin     = getSrcLine loc
        col     = getSrcColumn loc
        loc_msg = show lin ++ ':' : show col
    in
    loc_msg ++ ": " ++ msg
