--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Symbol Table datatype
-- It is used for type checking and for code generation
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module SymbolTable (
    -- * Table
    Table,          -- Abstract
    VarInfo(..), FunInfo(..),

    -- ** Constructing Table
    predefinedTable,

    -- ** Extract from Table
    getName, getCurrDepth,
    getLocalFuncs, getFunc, getFuncName, getFuncParams, getFuncRetType,
    getLocalVars, getVar, getVarName, getVarType, isVarLocal,

    -- ** Add to Table
    addFunc, addVar, updateFunc,
    updateUnusedFun, updateUnusedVar,

    -- ** Scopes
    rawOpenScope, rawCloseScope
  ) where

import TypedAst (AType(..), TType(..))
import UnTypedAst (Ide)
import SrcLoc
import Outputable (panic)

import qualified Data.Map as Map


-- -------------------------------------------------------------------
-- Symbol Table

data VarInfo = VarInfo {
    varName     :: Located Ide, -- variable location and name
    varType     :: AType,       -- variable type
    varId       :: !Int,        -- variable unique id
    varUnused   :: !Bool        -- variable is unused
  }

data FunInfo = FunInfo {
    funName     :: Located Ide, -- function location and name
    funParType  :: [AType],     -- parameters types
    funRetType  :: AType,       -- return type
    funId       :: !Int,        -- function unique id
    funUnused   :: !Bool        -- function is unused
  }

data Table = Table {
    depth       :: Int,         -- current nesting depth
    parent      :: Maybe Table, -- parent scope

    variables   :: Map.Map      -- local variables
                        Ide         -- name
                        VarInfo,    -- variable informations

    functions   :: Map.Map      -- local functions
                        Ide         -- name
                        FunInfo,    -- function informations

    name        :: Ide          -- defined function
  }


-- -------------------------------------------------------------------
-- Table functionality

-- strict library SrcSpan constructor:
{-# INLINE lL #-}
lL :: a -> Located a
lL a =
    let lspan = mkGeneralSrcSpan "<library function>"
    in lspan `seq` a `seq` L lspan a

predefinedTable :: Table
predefinedTable =
    Table 0 Nothing Map.empty
        (( -- Input/output
          Map.insert "writeInteger"
                (FunInfo (lL "writeInteger") [AType TTypeInt] (AType TTypeProc) 0 False) .
          Map.insert "writeByte"
                (FunInfo (lL "writeByte") [AType TTypeChar] (AType TTypeProc) 0 False) .
          Map.insert "writeChar"
                (FunInfo (lL "writeChar") [AType TTypeChar] (AType TTypeProc) 0 False) .
          Map.insert "writeString"
                (FunInfo (lL "writeString") [AType (TTypePtr TTypeChar)] (AType TTypeProc) 0 False) .
          Map.insert "readInteger"
                (FunInfo (lL "readInteger") [] (AType TTypeInt) 0 False) .
          Map.insert "readByte"
                (FunInfo (lL "readByte") [] (AType TTypeChar) 0 False) .
          Map.insert "readChar"
                (FunInfo (lL "readChar") [] (AType TTypeChar) 0 False) .
          Map.insert "readString"
                (FunInfo (lL "readString") [AType TTypeInt, AType (TTypePtr TTypeChar)]
                                                (AType TTypeProc) 0 False) .
           -- conversions
          Map.insert "extend"
                (FunInfo (lL "extend") [AType TTypeChar] (AType TTypeInt) 0 False) .
          Map.insert "shrink"
                (FunInfo (lL "shrink") [AType TTypeInt] (AType TTypeChar) 0 False) .
           -- strings
          Map.insert "strlen"
                (FunInfo (lL "strlen") [AType (TTypePtr TTypeChar)] (AType TTypeInt) 0 False) .
          Map.insert "strcmp"
                (FunInfo (lL "strcmp") [AType (TTypePtr TTypeChar), AType (TTypePtr TTypeChar)]
                                                (AType TTypeInt) 0 False) .
          Map.insert "strcpy"
                (FunInfo (lL "strcpy") [AType (TTypePtr TTypeChar), AType (TTypePtr TTypeChar)]
                                                (AType TTypeProc) 0 False) .
          Map.insert "strcat"
                (FunInfo (lL "strcat") [AType (TTypePtr TTypeChar), AType (TTypePtr TTypeChar)]
                                                (AType TTypeProc) 0 False)
         ) -- the end
          Map.empty
        ) "prelude"


-- search to all nested tables recursively
nested :: Table -> (Table -> Maybe a) -> Maybe a
nested t@Table{parent=pt} f =
    case f t of
         Just x -> Just x
         Nothing ->
             case pt of
                  Just p -> nested p f
                  Nothing -> Nothing

-- local function
getName :: Table -> Ide
getName Table{name=n} = n

getCurrDepth :: Table -> Int
getCurrDepth Table{depth=d} = d

-- Get functions
getLocalFuncs :: Table -> [FunInfo]
getLocalFuncs Table{functions=f} = Map.elems f

getFunc :: Ide -> Table -> Maybe FunInfo
getFunc i t =
    nested t (\Table{functions=f} -> Map.lookup i f)

getFuncName :: Maybe FunInfo -> Ide
getFuncName (Just (FunInfo n _ _ fid _)) = (unLoc n) ++ "_" ++ show fid
getFuncName Nothing = "unknown"

getFuncParams :: Maybe FunInfo -> [AType]
getFuncParams (Just (FunInfo _ fpt _ _ _)) = fpt
getFuncParams Nothing = []

getFuncRetType :: Maybe FunInfo -> AType
getFuncRetType (Just (FunInfo _ _ frt _ _)) = frt
getFuncRetType Nothing = AType TTypeUnknown

-- Get variables
getLocalVars :: Table -> [VarInfo]
getLocalVars Table{variables=v} = Map.elems v

getVar :: Ide -> Table -> Maybe VarInfo
getVar i t =
    nested t (\Table{variables=v} -> Map.lookup i v)

getVarName :: Maybe VarInfo -> Ide
getVarName (Just (VarInfo n _ vid _)) = (unLoc n) ++ "_" ++ show vid
getVarName Nothing = "unknown"

getVarType :: Maybe VarInfo -> AType
getVarType (Just (VarInfo _ vt _ _)) = vt
getVarType Nothing = AType TTypeUnknown

isVarLocal :: Ide -> Table -> Maybe VarInfo
isVarLocal i Table{variables=v} = Map.lookup i v

-- Add functions
addFunc :: Ide -> FunInfo -> Table -> Table
addFunc i fun_info t@Table{functions=f} =
    t{ functions=Map.insert i fun_info f }

-- Add variables
addVar :: Ide -> VarInfo -> Table -> Table
addVar i var_info t@Table{variables=v} =
    t{ variables=Map.insert i var_info v }

-- Update local function
updateFunc :: [AType] -> Table -> Table
updateFunc pt t@Table{name=fun_name, parent=m_parent} =
    case m_parent of
         Just parent@Table{functions=f} ->
             let parent' = parent{ functions=Map.update newVal fun_name f }
                 newVal (FunInfo lide _ rt u _) = Just (FunInfo lide pt rt u True)
             in
             t{ parent=(Just parent') }
         Nothing -> panic "SymbolTable.updateFunc is not supposed to be at outermost scope"

-- Update the unused `bit' of VarInfo/FunInfo
updateUnusedFun :: Ide -> Table -> Table
updateUnusedFun fun t@Table{parent=pt, functions=ft} =
    case Map.updateLookupWithKey (\_ fi -> Just fi{funUnused=False}) fun ft of
         (Just _, rf) -> t{functions=rf}
         (Nothing, _) ->
             case pt of
                  Just p -> t{parent = Just (updateUnusedFun fun p)}
                  Nothing ->
                      panic "SymbolTable.updateUnusedFun is not supposed to be at outermost scope"

updateUnusedVar :: Ide -> Table -> Table
updateUnusedVar var t@Table{parent=pt, variables=vt} =
    case Map.updateLookupWithKey (\_ vi -> Just vi{varUnused=False}) var vt of
         (Just _, rv) -> t{variables=rv}
         (Nothing, _) ->
             case pt of
                  Just p -> t{parent = Just (updateUnusedVar var p)}
                  Nothing ->
                      panic "SymbolTable.updateUnusedVar is not supposed to be at outermost scope"

-- Scopes
rawOpenScope :: Ide -> Table -> Table
rawOpenScope i t@Table{depth=d} =
    Table (d+1) (Just t) Map.empty Map.empty i

rawCloseScope :: Table -> Table
rawCloseScope Table{parent=p} =
    case p of
         Just t  -> t
         Nothing -> panic "SymbolTable.rawCloseScope cannot close outermost scope"
