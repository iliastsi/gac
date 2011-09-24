--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Symbol Table datatype
-- It is used for type checking and for code generation
--
--------------------------------------------------------------------------------

module SymbolTable where

import UnTypedAst

import qualified Data.Map as Map


-- -------------------------------------------------------------------
-- Symbol Table
data Table = Table {
    depth       :: Int,         -- current nesting depth
    parent      :: Maybe Table, -- parent scope

    variables   :: Map.Map      -- local variables
                        Ide         -- name (id)
                        UType,      -- types

    functions   :: Map.Map      -- local functions
                        Ide         -- name (id)
                        ([UType],   -- parameters types
                         UType),    -- return type

    name        :: Ide          -- defined function
  }


-- -------------------------------------------------------------------
-- Table functionality

initTable :: Ide -> Table
initTable i = Table 0 Nothing Map.empty Map.empty i

-- search to all nested tables recursively
nested :: Table -> (Table -> Maybe a) -> Maybe a
nested t@Table{parent=pt} f =
    case f t of
         Just x -> Just x
         Nothing ->
             case pt of
                  Just p -> nested p f
                  Nothing -> Nothing

getName :: Table -> Ide
getName t@Table{name=n} = n

getFuncParams :: Table -> Ide -> Maybe [UType]
getFuncParams t i =
    nested t (\Table{functions=f} -> Map.lookup i f >>= return . fst)

getFuncRetType :: Table -> Ide -> Maybe UType
getFuncRetType t i =
    nested t (\Table{functions=f} -> Map.lookup i f >>= return .snd)

getFuncDepth :: Table -> Ide -> Maybe Int
getFuncDepth t i =
    nested t (\Table{depth=d,functions=f} -> Map.lookup i f >> Just d)

getVarDepth :: Table -> Ide -> Maybe Int
getVarDepth t i =
    nested t (\Table{depth=d,variables=v} -> Map.lookup i v >> Just d)

getCurrDepth :: Table -> Int
getCurrDepth Table{depth=d} = d

getVarType :: Table -> Ide -> Maybe UType
getVarType t i =
    nested t (\Table{variables=v} -> Map.lookup i v)

isVarLocal :: Table -> Ide -> Bool
isVarLocal Table{variables=v} i =
    case Map.lookup i v of
         Just _  -> True
         Nothing -> False

addVar :: Table -> (Ide, UType) -> Table
addVar t@Table{variables=v} (i,dt) =
    t{ variables=Map.insert i dt v }

addFunc :: Table -> (Ide, [UType], UType) -> Table
addFunc t@Table{functions=f} (i, pt, rt) =
    t{ functions=Map.insert i (pt, rt) f }

rawOpenScope :: Ide -> Table -> Table
rawOpenScope i t@Table{depth=d} =
    Table (d+1) (Just t) Map.empty Map.empty i

rawCloseScope :: Table -> Table
rawCloseScope Table{parent=p} =
    case p of
         Just t  -> t
         Nothing -> error "cannot close outermost scope"
