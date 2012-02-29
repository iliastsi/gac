--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
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
    mkTcState, addTypeWarning, addTypeError,
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
        unique      = 0
    }

-- Errors/Warnings
addTypeWarning :: SrcSpan -> MsgCode -> String -> TcM ()
addTypeWarning loc msg_code msg_extra =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addWarning (mkWarnMsg loc msg_code msg_extra) msgs) } ()

addTypeError :: SrcSpan -> MsgCode -> String -> TcM ()
addTypeError loc msg_code msg_extra =
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg loc msg_code msg_extra) msgs) } ()

addRedefError :: Ide -> SrcSpan -> SrcSpan -> TcM ()
addRedefError ide curr prev = do
    let msg = ("Bound at: " ++ showSrcSpan prev ++ "\n\t" ++
               "          " ++ showSrcSpan curr)
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg curr (RedefError ide) msg) msgs) } ()


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
getFuncM lide@(L loc ide) = do
    t <- getTable
    case getFunc ide t of
         Just fi@FunInfo{funName=name, funUnused=isUnused} -> do
             when isUnused $ setTable (updateUnusedFun (unLoc name) t)
             return (Just fi)
         Nothing -> do
             addTypeError loc (ScopeError ide)
               "Each undeclared identifier is reported only once for each function it appears in"
             -- add the function
             u <- getUnique
             let finfo = FunInfo lide [] (AType TTypeUnknown) u False
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
getVarM lide@(L loc ide) = do
    t <- getTable
    case getVar ide t of
         Just vi@VarInfo{varName=name, varUnused=isUnused} -> do
             when isUnused $ setTable (updateUnusedVar (unLoc name) t)
             return (Just vi)
         Nothing -> do
             addTypeError loc (ScopeError ide)
               "Each undeclared identifier is reported only once for each function it appears in"
             -- add the variable
             u <- getUnique
             let vinfo = VarInfo lide (AType TTypeUnknown) u False
             setTable (addVar ide vinfo t)
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
         Just (FunInfo lprev _ _ _ _) ->
             addRedefError ide loc (getLoc lprev)
    -- we re-insert the function (in the current scope)
    -- in order to continue with the type checking
    u <- getUnique
    let finfo = FunInfo lide pt rt u True
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
             let vinfo = VarInfo lide vt u True
             setTable (addVar ide vinfo t)
             return (getVarName (Just vinfo))
         Just (VarInfo lprev _ _ _) -> do
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
rawCloseScopeM = do
    t <- getTable
    flags <- getDynFlags
    when (dopt Opt_WarnUnusedFunction flags) $
        mapM_ isUnusedFun $ getLocalFuncs t
    when (dopt Opt_WarnUnusedVariable flags) $
        mapM_ isUnusedVar $ getLocalVars t
    setTable (rawCloseScope t)

isUnusedVar :: VarInfo -> TcM ()
isUnusedVar (VarInfo _ _ _ False) = return ()
isUnusedVar (VarInfo (L loc vn) _ _ True)  =
    addTypeWarning loc (UnusedIdError vn) ""

isUnusedFun :: FunInfo -> TcM ()
isUnusedFun (FunInfo _ _ _ _ False) = return ()
isUnusedFun (FunInfo (L loc fn) _ _ _ True) =
    addTypeWarning loc (UnusedIdError fn) ""
