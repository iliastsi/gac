--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Generate llvm code
--
-- This module used llvm bindings for haskell to
-- compile our Typed AST into llvm code
--
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module LlvmCodeGen (compile) where

import TypedAst
import UnTypedAst
import Outputable

import qualified Data.Map as Map
import Prelude hiding (and, or)
import Data.Map (Map)
import Data.Int
import Data.Word
import LLVM.Core


-- -------------------------------------------------------------------
-- Some important data types to keep our Environment

data AValue = forall a. AValue (Value a) (TType a)
data AFunc = forall a. AFunc (Function a) (TType a)

type ValueEnv = (Ide, AValue)
type FuncEnv = (Ide, AFunc)
type Env = (Map Ide AFunc, Map Ide AValue)

-- We need this to solve a rigid-something error
data SArray = forall n. Nat n => SArray (Global (Array n Word8))

-- -------------------------------------------------------------------
-- Compile our module to LLVM
compile :: [TAst] -> CodeGenModule ()
compile tasts = do
    -- Firstly declare all the functions
    funD <- declareFun ExternalLinkage $ head tasts
    funD' <- mapM (declareFun InternalLinkage) $ tail tasts
    funD'' <- compilePrelude
    let fun_env = Map.fromList $ (funD:funD') ++ funD''
        var_env = Map.empty
        env = (fun_env, var_env)
    mapM_ (compileDefFun env) tasts


-- -------------------------------------------------------------------
-- Declare one function
declareFun :: Linkage -> ADefFun -> CodeGenModule FuncEnv
declareFun linkage (ADefFun tdef ttype) = declareFun' linkage tdef ttype

declareFun' :: forall a. (IsFunction a) =>
            Linkage -> TDefFun a -> TType a -> CodeGenModule FuncEnv
declareFun' linkage tdef ttype = do
    let f_name = funName tdef
    (f_value::Function a) <- newNamedFunction linkage f_name
    return (f_name, AFunc f_value ttype)

funName :: forall a. (IsFunction a) => TDefFun a -> Ide
funName (TDefPar _ _ tdef) = funName tdef
funName (TDefFunL ide _ _ _) = ide
funName _ = panic "LlvmCodeGen.funName got unexpected input"


-- -------------------------------------------------------------------
-- Compile TDefFun into llvm
compileDefFun :: Env -> ADefFun -> CodeGenModule ()
compileDefFun env@(fun_env,_) (ADefFun tdef t_type) = do
    let fname = funName tdef
    case Map.lookup fname fun_env of
         Just (AFunc f_value f_type) ->
             case test t_type f_type of
                  Just Eq -> do
                      defineFunction f_value (compDefFun env [] tdef)
                      return ()
                  Nothing -> panic "LlvmCodeGen.compileDefFun test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileDefFun lookup returned Nothing"

-- ---------------------------
compDefFun :: (IsFunction a, Translate a) => Env -> [ValueEnv] -> TDefFun a -> CG a
compDefFun env val_env (TDefFunL _ide ttype adefvar tstmt) = do
    case (test ttype TTypeInt, test ttype TTypeChar, test ttype TTypeProc) of
         (Just Eq, Nothing, Nothing) -> compDefFun' env val_env ttype adefvar tstmt
         (Nothing, Just Eq, Nothing) -> compDefFun' env val_env ttype adefvar tstmt
         (Nothing, Nothing, Just Eq) -> compDefFun' env val_env ttype adefvar tstmt
         _ -> panic "LlvmCodeGen.compDefFun test had to return Eq"
compDefFun env val_env (TDefPar ide ttype tdeffun) =
    \v_value -> compDefFun env ((ide, AValue v_value ttype):val_env) tdeffun
compDefFun _ _ _ = panic "LlvmCodeGen.compDefFun got unexpected input"

compDefFun' :: Env -> [ValueEnv] -> TType r -> [ADefVar] -> TStmt -> CodeGenFunction r ()
compDefFun' (fun_env,_) val_env ttype adefvar tstmt = do
    val_env' <- mapM (compileDefVar ttype) adefvar
    let val_env'' = Map.fromList (val_env ++ val_env')
        env' = (fun_env, val_env'')
    compileStmt env' ttype tstmt

-- -------------------------------------------------------------------
-- Compile TDefVar into llvm
compileDefVar :: TType r -> ADefVar -> CodeGenFunction r ValueEnv
compileDefVar _ (ADefVar (TDefVar ide vtype) _) = do
    let arr_size  = getArrSize vtype
        arr_size' = (fromIntegral $ head arr_size) :: Word32
        arr_type  = getVarType vtype
    case (test arr_type TTypeInt, test arr_type TTypeChar) of
         (Just Eq, Nothing) ->
             cmpVarAlloc ide arr_size' vtype
         (Nothing, Just Eq) ->
             cmpVarAlloc ide arr_size' vtype
         _ -> panic "LlvmCodeGen.compileDefVar test had to return Eq"

-- Helper function to allocate memory
cmpVarAlloc :: forall a r s. (IsSized a s, IsFirstClass a) =>
            Ide -> Word32 -> TType a -> CodeGenFunction r ValueEnv
cmpVarAlloc ide idx vtype = do
    (t::Value (Ptr a)) <- arrayAlloca idx
    return (ide, AValue t (TTypePtr vtype))


-- -------------------------------------------------------------------
-- Compile TStmt into llvm
compileStmt :: Env -> TType r -> TStmt -> CodeGenFunction r Terminate
-- TStmtNothing
compileStmt _env _rtype TStmtNothing = return ()
-- TStmtAssign
compileStmt env _rtype (TStmtAssign avar aexpr) = do
    (AVariable var vtype) <- return avar
    (AExpr expr etype) <- return aexpr
    v_value <- compileVariable env var
    e_value <- compileExpr env expr
    case test etype vtype of
         Just Eq -> store e_value v_value
         Nothing -> panic "LlvmCodeGen.compileStmt test had to return Eq"
-- TStmtCompound
compileStmt env rtype (TStmtCompound _ tstmts) =
    mapM_ (compileStmt env rtype) tstmts
-- TStmtFun
compileStmt env _rtype (TStmtFun afunc) = do
    AFuncCall func (TTypeRetIO _) <- return afunc
    t1 <- compileFuncCall env func
    _ <- t1
    return ()
-- TStmtIf
compileStmt env rtype (TStmtIf acond if_stmt melse_stmt) = do
    true <- newBasicBlock
    false <- newBasicBlock
    exit <- newBasicBlock
    ACond cond <- return acond
    t1 <- compileCond env cond
    condBr t1 true false
    -- cond was True
    defineBasicBlock true
    compileStmt env rtype if_stmt
    if doesStmtReturn if_stmt
       then return ()
       else br exit
    -- cond was False
    defineBasicBlock false
    case melse_stmt of
         Just else_stmt -> do
             compileStmt env rtype else_stmt
             if doesStmtReturn else_stmt
                then return ()
                else br exit
         Nothing -> br exit
    -- exit block
    defineBasicBlock exit
    return ()
-- TStmtWhile
compileStmt env rtype (TStmtWhile acond tstmt) = do
    loop <- newBasicBlock
    body <- newBasicBlock
    exit <- newBasicBlock
    ACond cond <- return acond
    br loop
    -- loop block
    defineBasicBlock loop
    t1 <- compileCond env cond
    condBr t1 body exit
    -- body block
    defineBasicBlock body
    compileStmt env rtype tstmt
    if doesStmtReturn tstmt
       then return ()
       else br loop
    -- exit block
    defineBasicBlock exit
    return ()
-- TStmtReturn
compileStmt env rtype (TStmtReturn maexpr) = do
    case maexpr of
         Just (AExpr expr etype) -> do
             case test etype rtype of
                  Just Eq -> do
                      t1 <- compileExpr env expr
                      ret t1
                  Nothing ->
                      panic "LlvmCodeGen.compileStmt test had to return Eq"
         Nothing -> do
             case test TTypeProc rtype of
                  Just Eq -> do
                      ret ()
                  Nothing ->
                      panic "LlvmCodeGen.compileStmt test had to return Eq"

-- ---------------------------
-- Check if a Stmt returns
doesStmtReturn :: TStmt -> Bool
doesStmtReturn (TStmtCompound has_ret _) = has_ret
doesStmtReturn (TStmtReturn _) = True
doesStmtReturn _ = False


-- -------------------------------------------------------------------
-- Compile TExpr into llvm
compileExpr :: Env -> TExpr a -> CodeGenFunction r (Value a)
-- TExprInt
compileExpr _ (TExprInt i)    = return $ valueOf i
-- TExprChar
compileExpr _ (TExprChar c)   = return $ valueOf c
-- TExprString
compileExpr _ (TExprString s) = do
    SArray msg <- liftCodeGenModule $ withStringNul s (return . SArray)
    getElementPtr msg (0::Word32, (0::Word32, ()))
-- TExprVar
compileExpr env (TExprVar v) = do
    t1 <- compileVariable env v
    load t1
-- TExprFun
compileExpr env (TExprFun tfun) = do
    t1 <- compileFuncCall env tfun
    t1
-- TExprMinus
compileExpr env (TExprMinus texpr) = do
    t1 <- compileExpr env texpr
    t2 <- return $ valueOf (0::Int32)
    sub t2 t1
-- TExprOp
compileExpr env (TExprOp e1 op e2 tt1) = do
    t1 <- compileExpr env e1
    t2 <- compileExpr env e2
    case (test tt1 TTypeInt, test tt1 TTypeChar) of
         (Just Eq, Nothing) -> cmpArithOp op t1 t2
         (Nothing, Just Eq) -> cmpArithOp op t1 t2
         _ -> panic "LlvmCodeGen.compileExpr test had to return Eq"

-- Arithmetic operations
cmpArithOp :: (IsInteger a) => Op -> Value a -> Value a -> CodeGenFunction r (Value a)
cmpArithOp OpPlus  = add
cmpArithOp OpMinus = sub
cmpArithOp OpTimes = mul
cmpArithOp OpDiv   = idiv
cmpArithOp OpMod   = irem
cmpArithOp _ = panic "LlvmCodeGen.cmpArithOp got unexpected input"


-- -------------------------------------------------------------------
-- Compile TCond into llvm
compileCond :: Env -> TCond Bool -> CodeGenFunction r (Value Bool)
-- TCondTrue
compileCond _ TCondTrue  = return $ valueOf True
-- TCondFalse
compileCond _ TCondFalse = return $ valueOf False
-- TCondNot
compileCond env (TCondNot tcond) = do
    t1 <- compileCond env tcond
    xor t1 True
-- TCondOp
compileCond env (TCondOp e1 op e2 tt1) = do
    t1 <- compileExpr env e1
    t2 <- compileExpr env e2
    case (test tt1 TTypeInt, test tt1 TTypeChar) of
         (Just Eq, Nothing) -> cmpCompOp op t1 t2
         (Nothing, Just Eq) -> cmpCompOp op t1 t2
         _ -> panic "LlvmCodeGen.compileCond test had to return Eq"
-- TCondLog
compileCond env tcond@(TCondLog {}) =
    cmpLogOp env tcond

-- Comparison operations
cmpCompOp :: forall a b c d r. (CmpRet c d, CmpOp a b c d) =>
          Op -> a -> b -> CodeGenFunction r (Value d)
cmpCompOp OpEqual    = cmp CmpEQ
cmpCompOp OpNotEqual = cmp CmpNE
cmpCompOp OpLT       = cmp CmpLT
cmpCompOp OpGT       = cmp CmpGT
cmpCompOp OpLE       = cmp CmpLE
cmpCompOp OpGE       = cmp CmpGE
cmpCompOp _ = panic "LlvmCodeGen.cmpCompOp got unexpected input"

-- Logical operations
-- We have to use short circuit evaluation
cmpLogOp :: Env -> TCond Bool -> CodeGenFunction r (Value Bool)
cmpLogOp env (TCondLog c1 OpAnd c2) = do
    top <- getCurrentBasicBlock
    true <- newBasicBlock
    false <- newBasicBlock
    t1 <- compileCond env c1
    condBr t1 true false
    -- c1 was True
    defineBasicBlock true
    t2 <- compileCond env c2
    br false
    -- c1 was False
    defineBasicBlock false
    phi [(t1, top), (t2, true)]
cmpLogOp env (TCondLog c1 OpOr c2) = do
    top <- getCurrentBasicBlock
    true <- newBasicBlock
    false <- newBasicBlock
    t1 <- compileCond env c1
    condBr t1 true false
    -- c1 was False
    defineBasicBlock false
    t2 <- compileCond env c2
    br true
    -- c1 was True
    defineBasicBlock true
    phi [(t1, top), (t2, false)]
cmpLogOp _ _ = panic "LlvmCodeGen.cmpLogOp got unexpected input"


-- -------------------------------------------------------------------
-- Compile TVariable into llvm
compileVariable :: (IsFirstClass a) =>
                Env -> TVariable a -> CodeGenFunction r (Value (Ptr a))
-- TVar
compileVariable (_,var_env) (TVar ide t_type) = do
    case Map.lookup ide var_env of
         Just (AValue v_value v_type) ->
             case test (TTypePtr t_type) v_type of
                  Just Eq -> return $ v_value
                  Nothing -> panic "LlvmCodeGen.compileVariable test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileVariable lookup returned Nothing"
-- TVarArray
compileVariable env tvar@(TVarArray {}) = do
    let tt1 = getArrType tvar
        ss1 = getArrSize tt1
    (_, v_value) <- compArray env tvar ss1
    return v_value
-- TVarPtr
compileVariable env (TVarPtr tvar) = do
    v_value <- compileVariable env tvar
    -- dummy store - load
    t1 <- alloca
    store v_value t1
    return t1

-- We use this function when compiling Arrays
compArray :: (IsFirstClass a) => Env -> TVariable a -> [Int32]
          -> CodeGenFunction r ([Int32], Value (Ptr a))
compArray (_,var_env) (TVar ide t_type) (_:idxs) = do
    case Map.lookup ide var_env of
         Just (AValue v_value v_type) ->
             case test (TTypePtr t_type) v_type of
                  Just Eq -> do
                      return (idxs, v_value)
                  Nothing -> panic "LlvmCodeGen.compArray test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileArray lookup returned Nothing"
compArray env (TVarArray tvar expr) idxs = do
    ((idx:idxs'), v_value) <- compArray env tvar idxs
    t1 <- compileExpr env expr
    t2 <- mul t1 (valueOf idx)
    t3 <- getElementPtr v_value (t2, ())
    return $ (idxs', t3)
compArray _ _ _ = panic "LlvmCodeGen.compArray got unexpected input"

-- Return the Type of a TVarArray
getArrType :: TVariable a -> TType a
getArrType (TVarArray tvar _) =
    getArrType tvar
getArrType (TVar _ t_type) = t_type
getArrType _ = panic "LlvmCodeGen.getArrType got unexpected input"

-- Return the Type of our variable
getVarType :: TType a -> TType a
getVarType (TTypeArr t_type _) =
    getVarType t_type
getVarType (TTypePtr _) = panic "LlvmCodeGen.getVarType got unexpected input"
getVarType t_type = t_type

-- Return the size of our array
getArrSize :: TType a -> [Int32]
getArrSize (TTypeArr t_type Nothing) =
    (1 : getArrSize t_type)
getArrSize (TTypeArr t_type (Just offset)) =
    let (a:as) = getArrSize t_type in
    (a*offset : a : as)
getArrSize _ = [1]


-- -------------------------------------------------------------------
-- Compile TFuncCall into llvm
compileFuncCall :: forall a f r. (CallArgs f a r) =>
                Env -> TFuncCall f -> CodeGenFunction r a
compileFuncCall (fun_env,_) (TFuncCall ide t_type) = do
    case Map.lookup ide fun_env of
         Just (AFunc f_value f_type) ->
             case test f_type t_type of
                  Just Eq -> return $ call f_value
                  Nothing -> panic "LlvmCodeGen.compileFuncCall test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileFuncCall lookup returned Nothing"
compileFuncCall env (TParamCall expr _ tfunc) = do
    t1 <- compileExpr env expr
    t2 <- compileFuncCall env tfunc
    return $ t2 t1


-- -------------------------------------------------------------------
-- Compile Prelude function definitions into llvm
compilePrelude :: CodeGenModule [FuncEnv]
compilePrelude = do
    t1  <- newNamedFunction ExternalLinkage "writeInteger" :: TFunction (Int32 -> IO ())
    t2  <- newNamedFunction ExternalLinkage "writeByte" :: TFunction (Word8 -> IO ())
    t3  <- newNamedFunction ExternalLinkage "writeChar" :: TFunction (Word8 -> IO ())
    t4  <- newNamedFunction ExternalLinkage "writeString" :: TFunction (Ptr Word8 -> IO ())
    t5  <- newNamedFunction ExternalLinkage "readInteger" :: TFunction (IO Int32)
    t6  <- newNamedFunction ExternalLinkage "readByte" :: TFunction (IO Word8)
    t7  <- newNamedFunction ExternalLinkage "readChar" :: TFunction (IO Word8)
    t8  <- newNamedFunction ExternalLinkage "readString" :: TFunction (Int32 -> Ptr Word8 -> IO ())
    t9  <- newNamedFunction ExternalLinkage "extend" :: TFunction (Word8 -> IO Int32)
    t10 <- newNamedFunction ExternalLinkage "shrink" :: TFunction (Int32 -> IO Word8)
    t11 <- newNamedFunction ExternalLinkage "strlen" :: TFunction (Ptr Word8 -> IO Int32)
    t12 <- newNamedFunction ExternalLinkage "strcmp" :: TFunction (Ptr Word8 -> Ptr Word8 -> IO Int32)
    t13 <- newNamedFunction ExternalLinkage "strcpy" :: TFunction (Ptr Word8 -> Ptr Word8 -> IO ())
    t14 <- newNamedFunction ExternalLinkage "strcat" :: TFunction (Ptr Word8 -> Ptr Word8 -> IO ())
    return $
      ("writeInteger", AFunc t1 $
        TTypeFuncR TTypeInt (TTypeRetIO TTypeProc))
      : ("writeByte", AFunc t2 $
          TTypeFuncR TTypeChar (TTypeRetIO TTypeProc))
      : ("writeChar", AFunc t3 $
          TTypeFuncR TTypeChar (TTypeRetIO TTypeProc))
      : ("writeString", AFunc t4 $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) (TTypeRetIO TTypeProc))
      : ("readInteger", AFunc t5 $
          TTypeRetIO TTypeInt)
      : ("readByte", AFunc t6 $
          TTypeRetIO TTypeChar)
      : ("readChar", AFunc t7 $
          TTypeRetIO TTypeChar)
      : ("readString", AFunc t8 $
          TTypeFuncR TTypeInt $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) (TTypeRetIO TTypeProc))
      : ("extend", AFunc t9 $
          TTypeFuncR TTypeChar (TTypeRetIO TTypeInt))
      : ("shrink", AFunc t10 $
          TTypeFuncR TTypeInt (TTypeRetIO TTypeChar))
      : ("strlen", AFunc t11 $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) (TTypeRetIO TTypeInt))
      : ("strcmp", AFunc t12 $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) (TTypeRetIO TTypeInt))
      : ("strcpy", AFunc t13 $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) (TTypeRetIO TTypeProc))
      : ("strcat", AFunc t14 $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) $
          TTypeFuncR (TTypePtr $ TTypeArr TTypeChar Nothing) (TTypeRetIO TTypeProc))
      : []
