module SymbolTable where

import AstTypes


-- Update function
update :: Eq a => (a, b) -> (a -> b) -> a -> b
update (x, v) f y = if x == y then v else f y

-- Symbol Table
data Table = Table {
    depth       :: Int,         -- current nesting depth
    parent      :: Maybe Table, -- parent scope

    variables   :: Ide ->               -- for local variables
                    Maybe (AST_type,    -- types
                           Int,         -- offsets
                           Bool),       -- is-reference flags

    functions   :: Ide ->               -- local functions
                    Maybe (Int,         -- total parameters size
                           [(AST_type,  -- parameters types
                             Bool)]),   -- is-reference flags

    funReturn   :: Ide ->               -- functions returning non-void
                    Maybe (Bool,        -- flags
                           AST_type),   -- and types

    funcId      :: Ide -> Maybe FuncID, -- identifiers for functions

    npo         :: Int,         -- next positive offset
    lno         :: Int,         -- last negative offset
    name        :: Maybe Ide    -- defined function
  }


-- Table functionality

emptyTable :: Table
emptyTable = Table 0 Nothing (\i->Nothing) (\i->Nothing)
                    (\i->Nothing) 8 0 Nothing

-- search to all nested tables recursively
nested :: Table -> (Table -> Maybe a) -> Maybe a
nested t@Table{parent=pt} f =
    case f t of
         Just x -> Just x
         Nothing ->
             case pt of
                  Just p -> nested p f
                  Nothing -> Nothing

getFuncId :: Table -> Ide -> Maybe FuncID
getFuncId t i = nested t (\Table{funcId=fd} -> fd i)

getName :: Table -> Maybe Ide
getName Table{name=n} = n

getFuncParamSize :: Table -> Ide -> Maybe Int
getFuncParamSize t i =
    nested t (\Table{functions=f} -> f i >>= \x -> return $ fst x)

getFuncParams :: Table -> Ide -> Maybe [(AST_type, Bool)]
getFuncParams t i =
    nested t (\Table{functions=f} -> f i >>= \x -> return $ snd x)

getFuncDepth :: Table -> Ide -> Maybe Int
getFuncDepth t i =
    nested t (\Table{depth=d,funcId=f} -> f i >> Just d)

getVarDepth :: Table -> Ide -> Maybe Int
getVarDepth t i =
    nested t (\Table{depth=d,variables=v} -> v i >> Just d)

getCurrDepth :: Table -> Int
getCurrDepth Table{depth=d} = d

getVarOffset :: Table -> Ide -> Maybe Int
getVarOffset t i =
    nested t (\Table{variables=v} -> v i >>= \(_,x,_) -> return x)

getVarType :: Table -> Ide -> Maybe AST_type
getVarType t i =
    nested t (\Table{variables=v} -> v i >>= \(x,_,_) -> return x)

isVarRef :: Table -> Ide -> Maybe Bool
isVarRef t i =
    nested t (\Table{variables=v} -> v i >>= \(_,_,x) -> return x)

isVarLocal :: Table -> Ide -> Bool
isVarLocal Table{variables=v} i =
    case v i of
         Just _ -> True
         Nothing -> False

addVar :: Table -> (Ide, AST_type) -> (Table, Bool)
addVar t@Table{variables=v,lno=l} (i,dt) =
    case v i of
         Just _  -> (t, False)
         Nothing -> let l' = l - sizeof dt
                        v' = update(i, Just(dt, l', False)) v
                    in
                    l' `seq` v' `seq` (t{variables=v',lno=l'}, True)

addPar :: Table -> (Ide, AST_type, AST_mode) -> (Table, Bool)
addPar t@Table{variables=v,npo=n} (i,dt,dm) =
    case v i of
         Just _ -> (t,False)
         Nothing -> let (size, flag) = case dm of
                                            AST_byval -> (sizeof dt, False)
                                            AST_byref -> (2, True)
                        n' = n + size
                        v' = update(i, Just(dt, n, flag)) v
                    in
                    (size, flag) `seq` n' `seq` v' `seq` (t{variables=v',npo=n'}, True)

addFunc :: Table -> FuncId ->
