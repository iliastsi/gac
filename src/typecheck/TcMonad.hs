--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- The TcMonad is a State Monad that keeps track of symbol table
-- and errors during the semantic analysis
-- 
--------------------------------------------------------------------------------

module TcMonad (
    -- Type check monad
    TcResult(..), TcState, TcM(..),
    failTcM, failSpanMsgTcM, failExprMsgTcM,
    getTcState, getTable, getUnique, setTable,
    getSrcSpan, setSrcSpan,
    mkTcState, addTcWarning, addTcError,
    getMessages,

    -- symbol table functionality
    getNameM, getCurrDepthM,
    getFuncNameM, getFuncParamsM, getFuncRetTypeM, getFuncDepthM,
    getVarNameM, getVarDepthM, getVarTypeM,
    addFuncM, addVarM,
    rawOpenScopeM, rawCloseScopeM
  ) where

import SrcLoc
import SymbolTable
import ErrUtils
import UnTypedAst (UType(..), UExpr, Ide)

import Control.Monad


-- -------------------------------------------------------------------
-- The Type Check Monad
data TcResult a
    = TcOk TcState a
    | TcFailed Messages

data TcState = TcState {
    table       :: Table,           -- the symbol table
    messages    :: Messages,        -- the error messages
    curr_span   :: SrcSpan,         -- current location
    unique      :: !Int             -- unique id number
  }

newtype TcM a = TcM { unTcM :: TcState -> TcResult a }

instance Monad TcM where
    return = returnTcM
    (>>=)  = thenTcM
    fail   = failTcM

returnTcM :: a -> TcM a
returnTcM a = a `seq` (TcM $ \s -> TcOk s a)

thenTcM :: TcM a -> (a -> TcM b) -> TcM b
(TcM m) `thenTcM` k = TcM $ \s ->
    case m s of
         TcOk s1  a    -> (unTcM (k a)) s1
         TcFailed msgs -> TcFailed msgs

failTcM :: String -> TcM a
failTcM msg = TcM $ \s@(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg noSrcSpan (TypeError Nothing) msg) ms)

failSpanMsgTcM :: SrcSpan -> String -> TcM a
failSpanMsgTcM span msg = TcM $ \s@(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg span (TypeError Nothing) msg) ms)

failExprMsgTcM :: SrcSpan -> Maybe UExpr -> String -> TcM a
failExprMsgTcM span expr msg = TcM $ \s@(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg span (TypeError expr) msg) ms)

getTcState :: TcM TcState
getTcState = TcM $  \s -> TcOk s s

getTable :: TcM Table
getTable = TcM $ \s@(TcState{table=t}) -> TcOk s t

getUnique :: TcM Int
getUnique = TcM $ \s@(TcState{unique=u}) -> TcOk s{unique=u+1} u

setTable :: Table -> TcM ()
setTable t = TcM $ \s -> TcOk s{table=t} ()

getSrcSpan :: TcM SrcSpan
getSrcSpan = TcM $ \s@(TcState{curr_span=span}) -> TcOk s span

setSrcSpan :: SrcSpan -> TcM ()
setSrcSpan span = TcM $ \s -> TcOk s{curr_span=span} ()

-- create a type check state
--
mkTcState :: Table -> TcState
mkTcState t =
    TcState {
        table       = t,
        messages    = emptyMessages,
        curr_span   = noSrcSpan,
        unique      = 1
    }

addTcWarning :: SrcSpan -> Maybe UExpr -> String -> TcM ()
addTcWarning loc expr msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addWarning (mkWarnMsg loc (TypeError expr) msg) msgs) } ()

addTcError :: SrcSpan -> Maybe UExpr -> String -> TcM ()
addTcError loc expr msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg loc (TypeError expr) msg) msgs) } ()

getMessages :: TcState -> Messages
getMessages TcState{messages=ms} = ms


----------------------------------------------------------------------
-- Symbol Table functionality

-- execute the function inside our state monad and on error add
-- ScopeError and return the given value
action :: (Table -> Maybe a) -> a -> String -> TcM a
action f ret msg =
    TcM $ \s@(TcState{messages=msgs,table=t,curr_span=span}) ->
        case f t of
             Just x  -> TcOk s x
             Nothing ->
                 TcOk s{messages=(addError (mkErrMsg span (ScopeError msg) "") msgs)} ret

-- local function
getNameM :: TcM Ide
getNameM = liftM getName getTable

getCurrDepthM :: TcM Int
getCurrDepthM = liftM getCurrDepth getTable

-- Get functions
getFuncNameM :: Ide -> TcM String
getFuncNameM ide = liftM (getFuncName ide) getTable

getFuncParamsM :: Ide -> TcM [UType]
getFuncParamsM ide =
    action (getFuncParams ide) [] ide

getFuncRetTypeM :: Ide -> TcM UType
getFuncRetTypeM ide =
    action (getFuncRetType ide) UTypeUnknown ide

getFuncDepthM :: Ide -> TcM Int
getFuncDepthM ide =
    action (getFuncDepth ide) 0 ide

-- Get variables
getVarNameM :: Ide -> TcM String
getVarNameM ide = liftM (getVarName ide) getTable

getVarDepthM :: Ide -> TcM Int
getVarDepthM ide =
    action (getVarDepth ide) 0 ide

getVarTypeM :: Ide -> TcM UType 
getVarTypeM ide =
    action (getVarType ide) UTypeUnknown ide

-- Add functions
addFuncM :: Ide -> FunInfo -> TcM ()
addFuncM ide fun_info =
    TcM $ \s@TcState{table=t} -> TcOk s{table=addFunc ide fun_info t} ()

-- Add variables
addVarM :: Ide -> VarInfo -> TcM ()
addVarM ide var_info =
    TcM $ \s@TcState{table=t} -> TcOk s{table=addVar ide var_info t} ()

-- Scopes
rawOpenScopeM :: Ide -> TcM ()
rawOpenScopeM ide =
    TcM $ \s@TcState{table=t} -> TcOk s{table=rawOpenScope ide t} ()

rawCloseScopeM :: TcM ()
rawCloseScopeM =
    TcM $ \s@TcState{table=t} -> TcOk s{table=rawCloseScope t} ()
