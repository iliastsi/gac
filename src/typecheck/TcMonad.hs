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
    getTcMessages, Prototype, setPrototypes, getTcProtos,

    -- symbol table functionality
    getNameM, getCurrDepthM,
    getFuncM, getFuncNameM, getFuncParamsM, getFuncRetTypeM,
    getVarM, getVarNameM, getVarTypeM,
    addFuncM, addProtoM, addVarM, updateFuncM,
    rawOpenScopeM, rawCloseScopeM
  ) where

import SrcLoc
import SymbolTable
import ErrUtils
import UnTypedAst (Ide, Mode)
import TypedAst (AType(..), TType(..))
import DynFlags
import Util

import Control.Monad
import Data.Maybe


-- -------------------------------------------------------------------
-- The Type Check Monad
data TcResult a
    = TcOk TcState a
    | TcFailed Messages

data TcState = TcState {
    dflags      :: DynFlags,
    table       :: Table,           -- the symbol table
    messages    :: Messages,        -- the error messages
    unique      :: !Int,            -- unique id number
    -- here are the functions for which we had a prototype
    -- but not a definition (real name, (location,changed name))
    prototypes  :: [Prototype]
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

-- ---------------------------
-- Keep info for a prototype
-- (Location, real name, changed name)
type Prototype = (SrcSpan, Ide, Ide)

getPrototypes :: TcM [Prototype]
getPrototypes = TcM $ \s@(TcState{prototypes=pts}) -> TcOk s pts

setPrototypes :: [Prototype] -> TcM ()
setPrototypes pts = TcM $ \s -> TcOk s{prototypes=pts} ()

-- create a type check state
--
mkTcState :: DynFlags -> Table -> TcState
mkTcState flags t =
    TcState {
        dflags      = flags,
        table       = t,
        messages    = emptyMessages,
        unique      = 0,
        prototypes  = []
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
addRedefError ide loc1 loc2 = do
    let [prev,curr] = sortLe (\a b -> a<= b) [loc1, loc2]
        msg = ("Bound at: " ++ showSrcSpan prev ++ "\n\t" ++
               "          " ++ showSrcSpan curr)
    TcM $ \s@(TcState{messages=msgs}) ->
        TcOk s{ messages=(addError (mkErrMsg curr (RedefError ide) msg) msgs) } ()


-- ---------------------------
--
getTcMessages :: TcState -> Messages
getTcMessages TcState{messages=ms} = ms

getTcProtos :: TcState -> [Prototype]
getTcProtos TcState{prototypes=pts} = pts


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
             let finfo = FunInfo lide [] (AType TTypeUnknown) u False False
             setTable (addFunc ide finfo t)
             return Nothing

getFuncNameM :: Maybe FunInfo -> TcM Ide
getFuncNameM = return . getFuncName

getFuncParamsM :: Maybe FunInfo -> TcM [(AType,Mode)]
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
addFuncM :: Located Ide -> [(AType,Mode)] -> AType -> TcM Ide
addFuncM lide@(L loc ide) pt rt = do
    t <- getTable
    case isFuncLocal ide t of
         Just (FunInfo lprev _ _ _ _ False) -> do
             -- function is already defined
             addRedefError ide loc (getLoc lprev)
             u <- getUnique
             let finfo = FunInfo lide pt rt u True False
             setTable (addFunc ide finfo t)
             return (getFuncName (Just finfo))
         Just (FunInfo (L loc' _) pt' rt' u un True) -> do
             -- we have a function prototype defined
             when (pt' /= pt || rt' /= rt) $
                 addTypeError loc (ProtoError ide)
                    ("Bound at: " ++ showSrcSpan loc' ++ "\n\t" ++
                     "          " ++ showSrcSpan loc)
             let finfo = FunInfo lide pt' rt' u un False
             setTable (addFunc ide finfo t)
             return (getFuncName (Just finfo))
         Nothing -> do
             -- we define the function for the first time
             u <- getUnique
             let finfo = FunInfo lide pt rt u True False
             setTable (addFunc ide finfo t)
             return (getFuncName (Just finfo))

-- Add function prototype
addProtoM :: Located Ide -> [(AType,Mode)] -> AType -> TcM Ide
addProtoM lide@(L loc ide) pt rt = do
    t <- getTable
    case isFuncLocal ide t of
         Just (FunInfo lprev _ _ _ _ _) ->
             addRedefError ide loc (getLoc lprev)
         Nothing ->
             return ()
    u <- getUnique
    let finfo = FunInfo lide pt rt u True True
    setTable (addFunc ide finfo t)
    return (getFuncName (Just finfo))

-- ---------------------------
-- Add variables
addVarM :: Located Ide -> AType -> TcM Ide
addVarM lide@(L loc ide) vt = do
    t <- getTable
    case isVarLocal ide t of
         Just (VarInfo lprev _ _ _) ->
             addRedefError ide loc (getLoc lprev)
         Nothing -> return ()
    u <- getUnique
    let vinfo = VarInfo lide vt u True
    setTable (addVar ide vinfo t)
    return (getVarName (Just vinfo))

-- ---------------------------
-- Update local function's parameters info
updateFuncM :: [(AType,Mode)] -> TcM ()
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
    addProtos t
    setTable (rawCloseScope t)

isUnusedVar :: VarInfo -> TcM ()
isUnusedVar (VarInfo _ _ _ False) = return ()
isUnusedVar (VarInfo (L loc vn) _ _ True)  =
    addTypeWarning loc (UnusedIdError vn) ""

isUnusedFun :: FunInfo -> TcM ()
isUnusedFun (FunInfo _ _ _ _ False _) = return ()
isUnusedFun (FunInfo (L loc fn) _ _ _ True _) =
    addTypeWarning loc (UnusedIdError fn) ""

-- ---------------------------
-- Handle prototypes
getProtos :: FunInfo -> Maybe Prototype
getProtos (FunInfo _ _ _ _ _ False ) = Nothing
getProtos finfo@(FunInfo (L loc fn) _ _ _ _ True) =
    let changed_name = getFuncName $ Just finfo in
    Just (loc, fn, changed_name)

addProtos :: Table -> TcM ()
addProtos t = do
    protos <- getPrototypes
    let protos' = catMaybes $ map getProtos $ getLocalFuncs t
    mapM_ (\(loc, ide, _) ->
        case lookupWith (\(_, ide', _) -> ide==ide') protos of
             Just (loc', _, _) -> addRedefError ide loc loc'
             Nothing -> return ()
        ) protos'
    setPrototypes (protos' ++ protos)
    return ()
