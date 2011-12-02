--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- The TcMonad is a State Monad that keeps track of symbol table
-- and errors during the semantic analysis
-- 
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module TcMonad (
    -- Type check monad
    TcResult(..), TcState, TcM(..),
    failTcM, failSpanMsgTcM, failExprMsgTcM,
    getTcState, getDynFlags, getTable, getUnique, setTable,
    mkTcState, addTypeWarning, addTypeError, addScopeError,
    addUnreachWarning, addNoRetError, addOverflowWarn,
    getTcMessages,

    -- symbol table functionality
    getNameM, getCurrDepthM,
    getFuncM, getFuncNameM, getFuncParamsM, getFuncRetTypeM,
    getVarM, getVarNameM, getVarTypeM,
    addFuncM, addVarM, updateFuncM,
    rawOpenScopeM, rawCloseScopeM
  ) where

import SrcLoc
import SymbolTable
import ErrUtils
import UnTypedAst (Ide)
import TypedAst (AType(..), TType(..))
import DynFlags

import Control.Monad


-- -------------------------------------------------------------------
-- The Type Check Monad
data TcResult a
    = TcOk TcState a
    | TcFailed Messages

data TcState = TcState {
    dflags      :: DynFlags,
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
failTcM msg = TcM $ \(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg noSrcSpan UnknownError msg) ms)

failSpanMsgTcM :: SrcSpan -> String -> TcM a
failSpanMsgTcM loc msg = TcM $ \(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg loc UnknownError msg) ms)

failExprMsgTcM :: SrcSpan -> String -> String -> TcM a
failExprMsgTcM loc expr msg = TcM $ \(TcState{messages=ms}) ->
    TcFailed (addError (mkErrMsg loc (TypeError expr) msg) ms)

getTcState :: TcM TcState
getTcState = TcM $ \s -> TcOk s s

getDynFlags :: TcM DynFlags
getDynFlags = TcM $ \s -> TcOk s (dflags s)

getTable :: TcM Table
getTable = TcM $ \s@(TcState{table=t}) -> TcOk s t

getUnique :: TcM Int
getUnique = TcM $ \s@(TcState{unique=u}) -> TcOk s{unique=u+1} u

setTable :: Table -> TcM ()
setTable t = TcM $ \s -> TcOk s{table=t} ()

-- create a type check state
--
mkTcState :: DynFlags -> Table -> TcState
mkTcState flags t =
    TcState {
        dflags      = flags,
        table       = t,
        messages    = emptyMessages,
        unique      = 1
    }

addTypeWarning :: SrcSpan -> String -> String -> TcM ()
addTypeWarning loc expr msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addWarning (mkWarnMsg loc (TypeError expr) msg) msgs) } ()

addTypeError :: SrcSpan -> String -> String -> TcM ()
addTypeError loc expr msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg loc (TypeError expr) msg) msgs) } ()

addScopeError :: Located Ide -> String -> TcM ()
addScopeError (L loc ide) msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg loc (ScopeError ide) msg) msgs) } ()

addUnreachWarning :: SrcSpan -> String -> TcM ()
addUnreachWarning loc msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addWarning (mkWarnMsg loc UnreachError msg) msgs) } ()

addRedefError :: Ide -> SrcSpan -> SrcSpan -> TcM ()
addRedefError ide curr prev = do
    let msg = ("Bound at: " ++ show prev ++ "\n\t" ++
               "          " ++ show curr)
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg curr (RedefError ide) msg) msgs) } ()

addNoRetError :: SrcSpan -> Ide -> String -> TcM ()
addNoRetError loc ide msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg loc (NoRetError ide) msg) msgs) } ()

-- Warn for type overflows
addOverflowWarn :: SrcSpan -> String -> String -> TcM ()
addOverflowWarn loc constant msg =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addWarning (mkWarnMsg loc (OverflowError constant) msg) msgs) } ()

getTcMessages :: TcState -> Messages
getTcMessages TcState{messages=ms} = ms


----------------------------------------------------------------------
-- Symbol Table functionality

-- ---------------------------
-- local function
getNameM :: TcM Ide
getNameM = liftM getName getTable

getCurrDepthM :: TcM Int
getCurrDepthM = liftM getCurrDepth getTable

-- ---------------------------
-- Get functions
getFuncM :: Located Ide -> TcM (Maybe FunInfo)
getFuncM lide@(L _ ide) = do
    t <- getTable
    case getFunc ide t of
         Just fi -> return (Just fi)
         Nothing -> do
             addScopeError lide "Each undeclared identifier is reported only once for each function it appears in"
             -- add the function
             u <- getUnique
             let finfo = FunInfo lide [] (AType TTypeUnknown) u
             setTable (addFunc ide finfo t)
             return Nothing

getFuncNameM :: Maybe FunInfo -> TcM Ide
getFuncNameM = return . getFuncName

getFuncParamsM :: Maybe FunInfo -> TcM [AType]
getFuncParamsM = return . getFuncParams

getFuncRetTypeM :: Maybe FunInfo -> TcM AType
getFuncRetTypeM = return . getFuncRetType

-- ---------------------------
-- Get variables
getVarM :: Located Ide -> TcM (Maybe VarInfo)
getVarM lide@(L _ ide) = do
    mvi <- liftM (getVar ide) getTable
    case mvi of
         Just vi -> return (Just vi)
         Nothing -> do
             addScopeError lide "Each undeclared identifier is reported only once for each function it appears in"
             -- add the variable
             _ <- addVarM lide (AType TTypeUnknown)
             return Nothing

getVarNameM :: Maybe VarInfo -> TcM Ide
getVarNameM = return . getVarName

getVarTypeM :: Maybe VarInfo -> TcM AType
getVarTypeM = return . getVarType

-- ---------------------------
-- Add functions
addFuncM :: Located Ide -> [AType] -> AType -> TcM Ide
addFuncM lide@(L loc ide) pt rt = do
    t <- getTable
    case getFunc ide t of
         Nothing ->
             return ()
         Just (FunInfo lprev _ _ _) ->
             addRedefError ide loc (getLoc lprev)
    -- we re-insert the function (in the current scope)
    -- in order to continue with the type checking
    u <- getUnique
    let finfo = FunInfo lide pt rt u
    setTable (addFunc ide finfo t)
    return (getFuncName (Just finfo))

-- ---------------------------
-- Add variables
addVarM :: Located Ide -> AType -> TcM Ide
addVarM lide@(L loc ide) vt = do
    t <- getTable
    case isVarLocal ide t of
         Nothing -> do
             u <- getUnique
             let vinfo = VarInfo lide vt u
             setTable (addVar ide vinfo t)
             return (getVarName (Just vinfo))
         Just (VarInfo lprev _ _) -> do
             -- Here we don't re-insert the variable
             addRedefError ide loc (getLoc lprev)
             return "unknown"

-- ---------------------------
-- Update local function's parameters info
updateFuncM :: [AType] -> TcM ()
updateFuncM pt = do
    t <- getTable
    setTable (updateFunc pt t)

-- ---------------------------
-- Scopes
rawOpenScopeM :: Ide -> TcM ()
rawOpenScopeM ide =
    TcM $ \s@TcState{table=t} -> TcOk s{table=rawOpenScope ide t} ()

rawCloseScopeM :: TcM ()
rawCloseScopeM =
    TcM $ \s@TcState{table=t} -> TcOk s{table=rawCloseScope t} ()
