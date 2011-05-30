--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- The GacMonad is a State Monad that keeps track of symbol table and errors
-- during the compilation time.
-- 
--------------------------------------------------------------------------------

module GacMonad where

import SrcLoc
import SymbolTable
import ErrUtils
import Uast
import Tast


data GResult a
    = GOk GState a
    | GFailed
      SrcLoc        -- The beginning of the text span related to the error
      Message       -- The error message

data GState = GState {
    table       :: Table,       -- the symbol table
    unique      :: Int,         -- unique number
    messages    :: Messages     -- the error messages
  }

newtype GacMonad a = GacMonad { unG :: GState -> GResult a }

instance Monad GacMonad where
    return = returnG
    (>>=)  = thenG
    fail   = failG

returnG :: a -> GacMonad a
returnG a = a `seq` (GacMonad $ \s -> GOk s a)

thenG :: GacMonad a -> (a -> GacMonad b) -> GacMonad b
(GacMonad m) `thenG` k = GacMonad $ \s ->
    case m s of
         GOk s1 a        -> (unG (k a)) s1
         GFailed loc msg -> GFailed loc msg

failG :: String -> GacMonad a
failG msg = GacMonad $ \_ -> GFailed (mkSrcLoc "stdin" 1 1) msg

failLocMsgG :: SrcLoc -> String -> GacMonad a
failLocMsgG loc msg = GacMonad $ \_ -> GFailed loc msg

mkGState :: Table -> Int -> GState
mkGState t u =
    GState {
        table       = t,
        unique      = u,
        messages    = emptyMessages
    }

getGState :: GacMonad GState
getGState = GacMonad $ \s -> GOk s s

getTable :: GacMonad Table
getTable = GacMonad $ \s@GState{table=t} -> GOk s t

getUnique :: GacMonad Int
getUnique = GacMonad $ \s@GState{unique=u} -> GOk s{unique=u+1} u

getMessages :: GacMonad Messages
getMessages = GacMonad $ \s@GState{messages=msg} -> GOk s msg

setTable :: Table -> GacMonad ()
setTable t = GacMonad $ \s -> GOk s{table=t} ()

--addWarning :: Messages -> SrcLoc -> String -> Messages
addWarning :: Messages -> String -> Messages
addWarning (ws,es) msg =
    let warning' = mkWarnMsg msg
        ws'      = ws `snocBag` warning'
    in (ws', es)

--addErro :: Messages -> SrcLoc -> String -> Messages
addError :: Messages -> String -> Messages
addError (ws,es) msg =
    let err' = mkErrMsg msg
        es'  = es `snocBag` err'
    in (ws, es')

addWarningM :: String -> GacMonad ()
addWarningM msg = GacMonad $ \s@GState{messages=pm} ->
                    GOk s{messages=(addWarning pm msg)} ()

addErrorM :: String -> GacMonad ()
addErrorM msg = GacMonad $ \s@GState{messages=pm} ->
                    GOk s{messages=(addError pm msg)} ()

----------------------------------------------------------------------
-- Symbol Table functionality

getNameM :: GacMonad Ide
getNameM = GacMonad $ \s@GState{table=t} -> GOk s (getName t)

getFuncParamsM :: Ide -> GacMonad [AType]
getFuncParamsM ide = GacMonad $ \s@GState{table=t,messages=pm} ->
                    case getFuncParams t ide of
                         Just x  -> GOk s x
                         Nothing -> GOk s{messages=(addError pm "Function not found")} []

getFuncRetTypeM :: Ide -> GacMonad AType
getFuncRetTypeM ide = GacMonad $ \s@GState{table=t,messages=pm} ->
                    case getFuncRetType t ide of
                         Just x  -> GOk s x
                         Nothing -> GOk s{messages=(addError pm "Function not found")} (AType (TTypeInt))

getFuncDepthM :: Ide -> GacMonad Int
getFuncDepthM ide = GacMonad $ \s@GState{table=t,messages=pm} ->
                    case getFuncDepth t ide of
                         Just x  -> GOk s x
                         Nothing -> GOk s{messages=(addError pm "Function not found")} 0

getVarDepthM :: Ide -> GacMonad Int
getVarDepthM ide = GacMonad $ \s@GState{table=t,messages=pm} ->
                    case getVarDepth t ide of
                         Just x  -> GOk s x
                         Nothing -> GOk s{messages=(addError pm "Variable not found")} 0

getCurrDepthM :: GacMonad Int
getCurrDepthM = GacMonad $ \s@GState{table=t} -> GOk s (getCurrDepth t)

getVarTypeM :: Ide -> GacMonad AType
getVarTypeM ide = GacMonad $ \s@GState{table=t,messages=pm} ->
                    case getVarType t ide of
                         Just x  -> GOk s x
                         Nothing -> GOk s{messages=(addError pm "Variable not found")} (AType (TTypeInt))

isVarLocalM :: Ide -> GacMonad Bool
isVarLocalM ide = GacMonad $ \s@GState{table=t} -> GOk s (isVarLocal t ide)

addVarM :: (Ide, AType) -> GacMonad ()
addVarM var = GacMonad $ \s@GState{table=t} ->
                    GOk s{table=(addVar t var)} ()

addFuncM :: (Ide, [AType], AType) -> GacMonad ()
addFuncM fun = GacMonad $ \s@GState{table=t} ->
                    GOk s{table=(addFunc t fun)} ()

rawOpenScopeM :: Ide -> GacMonad ()
rawOpenScopeM ide = GacMonad $ \s@GState{table=t} ->
                    GOk s{table=(rawOpenScope ide t)} ()

rawCloseScopeM :: GacMonad ()
rawCloseScopeM = GacMonad $ \s@GState{table=t} ->
                    GOk s{table=(rawCloseScope t)} ()
