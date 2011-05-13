module ErrUtils where

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
mkWarnMsg = mkErrMsg

mkErrMsg :: String -> ErrMsg
mkErrMsg msg = msg

snocBag :: Bag Message -> Message -> Bag Message
snocBag bag msg = bag ++ [msg]
