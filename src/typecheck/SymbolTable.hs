--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Symbol Table datatype
-- It is used for type checking and for code generation
--
--------------------------------------------------------------------------------

module SymbolTable (
    -- * Table
    Table,          -- Abstract
    VarInfo(..), FunInfo(..),

    -- ** Constructing Table
    initTable,

    -- ** Extract from Table
    getName, getCurrDepth,
    getFuncName, getFuncParams, getFuncRetType, getFuncId, getFuncDepth,
    getVarName, getVarDepth, getVarType, getVarId, isVarLocal,

    -- ** Add to Table
    addFunc, addVar,

    -- ** Scopes
    rawOpenScope, rawCloseScope
  ) where

import TypedAst (AType(..), TType(..))
import UnTypedAst (Ide)

import qualified Data.Map as Map


-- -------------------------------------------------------------------
-- Symbol Table

data VarInfo = VarInfo {
    varType     :: AType,       -- variable type
    varId       :: !Int         -- variable unique id
  }

data FunInfo = FunInfo {
    funParType  :: [AType],     -- parameters types
    funRetType  :: AType,       -- return type
    funId       :: !Int         -- function unique id
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

initTable :: Ide -> Table
initTable i = Table 1 (Just predefinedTable) Map.empty Map.empty i

predefinedTable :: Table
predefinedTable =
    Table 0 Nothing Map.empty
        (( -- Input/output
          Map.insert "writeInteger" (FunInfo [AType TTypeInt]  (AType TTypeProc) 0) .
          Map.insert "writeByte"    (FunInfo [AType TTypeChar] (AType TTypeProc) 0) .
          Map.insert "writeChar"    (FunInfo [AType TTypeChar] (AType TTypeProc) 0) .
          Map.insert "writeString"  (FunInfo [AType (TTypeArray 0 TTypeChar)] (AType TTypeProc) 0) .
          Map.insert "readInteger"  (FunInfo []                (AType TTypeInt)  0) .
          Map.insert "readByte"     (FunInfo []                (AType TTypeChar) 0) .
          Map.insert "readChar"     (FunInfo []                (AType TTypeChar) 0) .
          Map.insert "readString"   (FunInfo [AType TTypeInt, AType (TTypeArray 0 TTypeChar)]
                                                (AType TTypeProc) 0) .
           -- conversions
          Map.insert "extend"       (FunInfo [AType TTypeChar] (AType TTypeInt)  0) .
          Map.insert "shrink"       (FunInfo [AType TTypeInt]  (AType TTypeChar) 0) .
           -- strings
          Map.insert "strlen"       (FunInfo [AType (TTypeArray 0 TTypeChar)] (AType TTypeInt) 0) .
          Map.insert "strcmp"       (FunInfo [AType (TTypeArray 0 TTypeChar),
                                                AType (TTypeArray 0 TTypeChar)] (AType TTypeInt) 0) .
          Map.insert "strcpy"       (FunInfo [AType (TTypeArray 0 TTypeChar),
                                                AType (TTypeArray 0 TTypeChar)] (AType TTypeProc) 0) .
          Map.insert "strcat"       (FunInfo [AType (TTypeArray 0 TTypeChar),
                                                AType (TTypeArray 0 TTypeChar)] (AType TTypeProc) 0)
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
getName t@Table{name=n} = n

getCurrDepth :: Table -> Int
getCurrDepth Table{depth=d} = d

-- Get functions
getFuncName :: Ide -> Table -> String
getFuncName i t =
    case getFuncId i t of
         Just fid -> i ++ "_" ++ show fid
         Nothing  -> i

getFuncParams :: Ide -> Table -> Maybe [AType]
getFuncParams i t =
    nested t (\Table{functions=f} -> Map.lookup i f >>= return . funParType)

getFuncRetType :: Ide -> Table -> Maybe AType
getFuncRetType i t =
    nested t (\Table{functions=f} -> Map.lookup i f >>= return . funRetType)

getFuncId :: Ide -> Table -> Maybe Int
getFuncId i t =
    nested t (\Table{functions=f} -> Map.lookup i f >>= return . funId)

getFuncDepth :: Ide -> Table -> Maybe Int
getFuncDepth i t =
    nested t (\Table{depth=d,functions=f} -> Map.lookup i f >> Just d)

-- Get variables
getVarName :: Ide -> Table -> String
getVarName i t =
    case getVarId i t of
         Just vid -> i ++ "_" ++ show vid
         Nothing  -> i

getVarDepth :: Ide -> Table -> Maybe Int
getVarDepth i t =
    nested t (\Table{depth=d,variables=v} -> Map.lookup i v >> Just d)

getVarType :: Ide -> Table -> Maybe AType
getVarType i t =
    nested t (\Table{variables=v} -> Map.lookup i v >>= return . varType)

getVarId :: Ide -> Table -> Maybe Int
getVarId i t =
    nested t (\Table{variables=v} -> Map.lookup i v >>= return . varId)

isVarLocal :: Ide -> Table -> Bool
isVarLocal i Table{variables=v} =
    case Map.lookup i v of
         Just _  -> True
         Nothing -> False

-- Add functions
addFunc :: Ide -> FunInfo -> Table -> Table
addFunc i fun_info t@Table{functions=f} =
    t{ functions=Map.insert i fun_info f }

-- Add variables
addVar :: Ide -> VarInfo -> Table -> Table
addVar i var_info t@Table{variables=v} =
    t{ variables=Map.insert i var_info v }

-- Scopes
rawOpenScope :: Ide -> Table -> Table
rawOpenScope i t@Table{depth=d} =
    Table (d+1) (Just t) Map.empty Map.empty i

rawCloseScope :: Table -> Table
rawCloseScope Table{parent=p} =
    case p of
         Just t  -> t
         Nothing -> error "cannot close outermost scope"
