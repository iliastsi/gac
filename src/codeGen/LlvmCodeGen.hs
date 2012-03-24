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
import UnTypedAst (Ide)
import SrcLoc
import Outputable

import qualified Data.Map as Map
import Prelude hiding (and, or)
import Data.Map (Map)
import Data.Int
import Data.Word
import LLVM.Core


-- -------------------------------------------------------------------
-- Some important data types to keep our Environment

data AValue = forall a . AValue (Value a) (TType a)
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
    let funEnv = Map.fromList (funD:funD')
    return ()


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
funName (TDefFunL lide _ _ _) = unLoc lide
funName _ = panic "LlvmCodeGen.funName got unexpected input"


-- -------------------------------------------------------------------
-- Compile TDefVar into llvm
compileDefVar :: ADefVar -> CodeGenFunction r ValueEnv
compileDefVar (ADefVar (TDefVar lide vtype) _) =
    case (test vtype TTypeInt, test vtype TTypeChar) of
         (Just Eq, Nothing) ->
             cmpVarAlloc (unLoc lide) vtype
         (Nothing, Just Eq) ->
             cmpVarAlloc (unLoc lide) vtype
         _ -> panic "LlvmCodeGen.compileDefVar test had to return Eq"

-- Helper function to allocate memory
cmpVarAlloc :: forall a r s. (IsSized a s, IsFirstClass a, Type a) =>
            Ide -> TType a -> CodeGenFunction r ValueEnv
cmpVarAlloc ide vtype = do
    (t::Value (Ptr a)) <- alloca
    return (ide, AValue t (TTypePtr vtype))

-- Allocate array memory


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
compileStmt env rtype (TStmtCompound tstmts) =
    mapM_ (compileStmt env rtype) tstmts
-- TStmtFun
compileStmt env _rtype (TStmtFun afunc) = do
    AFuncCall func (TTypeRetIO ftype) <- return afunc
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
    br exit
    -- cond was False
    defineBasicBlock false
    case melse_stmt of
         Just else_stmt -> compileStmt env rtype else_stmt
         Nothing -> return ()
    br exit
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
    br loop
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
compileExpr env (TExprOp e1 op e2) = do
    t1 <- compileExpr env e1
    t2 <- compileExpr env e2
    let tt1 = getExprType e1
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

-- Return the type of a TExpr
getExprType :: (Type a) => TExpr a -> TType a
getExprType _ = theType


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
compileCond env (TCondOp e1 op e2) = do
    t1 <- compileExpr env e1
    t2 <- compileExpr env e2
    let tt1 = getExprType e1
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
compileVariable :: Env -> TVariable a -> CodeGenFunction r (Value (Ptr a))
-- TVar
compileVariable (_,var_env) (TVar ide t_type) = do
    case Map.lookup ide var_env of
         Just (AValue v_value v_type) ->
             case test (TTypePtr t_type) v_type of
                  Just Eq -> return $ v_value
                  Nothing -> panic "LlvmCodeGen.compileVariable test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileVariable lookup returned Nothing"
-- TVarArray
compileVariable env (TVarArray tvar offset) = do
    v_value <- compArray env tvar
    off_value <- compileExpr env offset
    getElementPtr v_value (off_value, ())
-- TVarPtr
compileVariable env (TVarPtr tvar) = do
    v_value <- compileVariable env tvar
    -- dummy store - load
    t1 <- alloca
    store v_value t1
    return t1

-- We use this function when compiling Arrays
compArray :: Env -> TVariable a -> CodeGenFunction r (Value a)
compArray (_,var_env) (TVar ide t_type) = do
    case Map.lookup ide var_env of
         Just (AValue v_value v_type) ->
             case test t_type v_type of
                  Just Eq -> return $ v_value
                  Nothing -> panic "LlvmCodeGen.compArray test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileArray lookup returned Nothing"
compArray env (TVarArray tvar offset) = do
    v_value <- compArray env tvar
    off_value <- compileExpr env offset
    t1 <- getElementPtr v_value (off_value, ())
    load t1
compArray _ _ = panic "LlvmCodeGen.compArray got unexpected input"


-- -------------------------------------------------------------------
-- Compile TFuncCall into llvm
compileFuncCall :: forall a f r. (CallArgs f a r) =>
                Env -> TFuncCall f -> CodeGenFunction r a
compileFuncCall (fun_env,_) (TFuncCall lide t_type) = do
    case Map.lookup (unLoc lide) fun_env of
         Just (AFunc f_value f_type) ->
             case test t_type f_type of
                  Just Eq -> return $ call f_value
                  Nothing -> panic "LlvmCodeGen.compileFuncCall test had to return Eq"
         Nothing -> panic "LlvmCodeGen.compileFuncCall lookup returned Nothing"
compileFuncCall env (TParamCall expr tfunc) = do
    t1 <- compileExpr env expr
    t2 <- compileFuncCall env tfunc
    return $ t2 t1
