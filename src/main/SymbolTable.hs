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
                    Maybe ((Int,        -- total parameters size
                            [(AST_type, -- parameters types
                              Bool)]),  -- is-reference flags
                           RetType),    -- return type

    npo         :: Int,         -- next positive offset
    lno         :: Int,         -- last negative offset
    name        :: Ide          -- defined function
  }

instance Show Table where
    showsPrec d (Table cd pt _ _ np ln df) =
        ("(depth=" ++) . showsPrec d cd .
        (", parent " ++) . (case pt of Just _ -> ("yes" ++); Nothing -> ("no" ++)) .
        (", npo=" ++) . showsPrec d np .
        (", lno=" ++) . showsPrec d ln .
        (", name=" ++) . showsPrec d df .
        (")" ++)


-- Table functionality

emptyTable :: Ide -> Table
emptyTable i = Table 0 Nothing (\_->Nothing) (\_->Nothing) 8 0 i

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
getName Table{name=n} = n

getFuncParamSize :: Table -> Ide -> Maybe Int
getFuncParamSize t i =
    nested t (\Table{functions=f} -> f i >>= return . fst . fst)

getFuncParams :: Table -> Ide -> Maybe [(AST_type, Bool)]
getFuncParams t i =
    nested t (\Table{functions=f} -> f i >>= return . snd . fst)

getFuncResult :: Table -> Ide -> RetType
getFuncResult t i =
    nested t (\Table{functions=f} -> f i >>= snd)

getFuncDepth :: Table -> Ide -> Maybe Int
getFuncDepth t i =
    nested t (\Table{depth=d,functions=f} -> f i >> Just d)

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
         Just _  -> True
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
         Just _  -> (t, False)
         Nothing -> let (size, flag) = case dm of
                                            AST_byval -> (sizeof dt, False)
                                            AST_byref -> (2, True)
                        n' = n + size
                        v' = update(i, Just(dt, n, flag)) v
                    in
                    (size, flag) `seq` n' `seq` v' `seq` (t{variables=v',npo=n'}, True)

addFunc :: Table -> (Ide, RetType, [(Ide, AST_type, AST_mode)]) -> (Table, Bool)
addFunc t@Table{functions=f} (i, rt, pl) =
    let processPar [] = (0, [])
        processPar ((_, dt, m) : ps) =
            let size = case m of
                            AST_byval -> sizeof dt
                            AST_byref -> 2
                (n, l) = processPar ps
            in  (n + size, (dt, m == AST_byref) : l)
    in
    case f i of
         Just _  -> (t, False)
         Nothing -> let f' = update (i, Just ((processPar pl),rt)) f
                    in
                    f' `seq` (t{functions=f'}, True)

rawOpenScope :: Ide -> Table -> Table
rawOpenScope i t@Table{depth=d} =
    Table (d+1) (Just t) (\_->Nothing) (\_-> Nothing) 8 0 i

rawCloseScope :: Table -> Table
rawCloseScope Table{parent=p} =
    case p of
         Just t  -> t
         Nothing -> error "cannot close outermost scope"
