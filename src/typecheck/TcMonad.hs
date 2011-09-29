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
import UnTypedAst (UExpr, Ide)
import TypedAst (AType(..), TType(..))

import Control.Monad


-- -------------------------------------------------------------------
-- The Type Check Monad
data TcResult a
    = TcOk TcState a
    | TcFailed Messages

data TcState = TcState {
    table       :: Table,           -- the symbol table
    messages    :: Messages,        -- the error messages
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
failSpanMsgTcM loc msg = TcM $ \s@(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg loc (TypeError Nothing) msg) ms)

failExprMsgTcM :: SrcSpan -> Maybe UExpr -> String -> TcM a
failExprMsgTcM loc expr msg = TcM $ \s@(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg loc (TypeError expr) msg) ms)

getTcState :: TcM TcState
getTcState = TcM $  \s -> TcOk s s

getTable :: TcM Table
getTable = TcM $ \s@(TcState{table=t}) -> TcOk s t

getUnique :: TcM Int
getUnique = TcM $ \s@(TcState{unique=u}) -> TcOk s{unique=u+1} u

setTable :: Table -> TcM ()
setTable t = TcM $ \s -> TcOk s{table=t} ()

-- create a type check state
--
mkTcState :: Table -> TcState
mkTcState t =
    TcState {
        table       = t,
        messages    = emptyMessages,
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
action :: (Table -> Maybe a) -> a -> SrcSpan -> String -> TcM a
action f ret loc msg =
    TcM $ \s@(TcState{messages=msgs,table=t}) ->
        case f t of
             Just x  -> TcOk s x
             Nothing ->
                 TcOk s{messages=(addError (mkErrMsg loc (ScopeError msg) "") msgs)} ret

-- local function
getNameM :: TcM Ide
getNameM = liftM getName getTable

getCurrDepthM :: TcM Int
getCurrDepthM = liftM getCurrDepth getTable

-- Get functions
getFuncNameM :: Located Ide -> TcM String
getFuncNameM (L _ ide) = liftM (getFuncName ide) getTable

getFuncParamsM :: Located Ide -> TcM [AType]
getFuncParamsM (L loc ide) =
    action (getFuncParams ide) [] loc ide

getFuncRetTypeM :: Located Ide -> TcM AType
getFuncRetTypeM (L loc ide) =
    action (getFuncRetType ide) (AType TTypeUnknown) loc ide

getFuncDepthM :: Located Ide -> TcM Int
getFuncDepthM (L loc ide) =
    action (getFuncDepth ide) 0 loc ide

-- Get variables
getVarNameM :: Located Ide -> TcM String
getVarNameM (L _ ide) = liftM (getVarName ide) getTable

getVarDepthM :: Located Ide -> TcM Int
getVarDepthM (L loc ide) =
    action (getVarDepth ide) 0 loc ide

getVarTypeM :: Located Ide -> TcM AType 
getVarTypeM (L loc ide) =
    action (getVarType ide) (AType TTypeUnknown) loc ide

-- Add functions
addFuncM :: Ide -> [AType] -> AType -> TcM ()
addFuncM ide pt rt = do
    u <- getUnique
    t <- getTable
    let finfo = FunInfo pt rt u
    setTable (addFunc ide finfo t)

-- Add variables
addVarM :: Ide -> AType -> TcM ()
addVarM ide vt = do
    u <- getUnique
    t <- getTable
    let vinfo = VarInfo vt u
    setTable (addVar ide vinfo t)

-- Scopes
rawOpenScopeM :: Ide -> TcM ()
rawOpenScopeM ide =
    TcM $ \s@TcState{table=t} -> TcOk s{table=rawOpenScope ide t} ()

rawCloseScopeM :: TcM ()
rawCloseScopeM =
    TcM $ \s@TcState{table=t} -> TcOk s{table=rawCloseScope t} ()
