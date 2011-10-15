--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Type check our abstruct syntax tree
--
-- The aim of the type checker is to transform from the UnTypedAst type to
-- the TypedAst type, so basicaly
--      typeCheckExpr :: UExpr -> TExpr a
--
-- We will use our TcM State Monad
--      typeCheckExpr :: UExpr -> TcM (TExpr a)
--
-- But wait! This type is totally wrong. Why? Because it promises that given a
-- UExpr the type checker can return any type, i.e., writing out the
-- (normally implicit) quantifier the type is:
--      typeCheckExpr :: forall a . UExpr -> TcM (TExpr a)
-- But this is not the case, the type checker will figure out a type and return
-- an expression with this specific type, so the type we really want is
--      typeCheckExpr :: exists a . UExpr -> TcM (TExpr a)
-- 
-- Haskell doesn't allow this type to be written this way; we need to package
-- up the existential type in a data type. Like so:
--      data AExpr = forall a . AExpr (TExpr a) (TType a)
--
-- Now our typeCheckExpr function has type
--      typeCheckExpr :: UExpr -> TcM (AExpr)
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, PatternGuards #-}

module TypeCheck where

import UnTypedAst
import TypedAst
import TcMonad
import SrcLoc
import SymbolTable

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


-- -------------------------------------------------------------------
-- TypeCheck UDef
typeCheckDef :: Located UDef -> TcM (Located ADef)
-- UDefFun (without parameters)
typeCheckDef (L loc (UDefFun lide [] lutype ludefs lustmt)) = do
    (L type_loc (AType ftype)) <- typeCheckType lutype
    fname <- liftM (L (getLoc lide)) (addFuncM lide [] (AType ftype))
    rawOpenScopeM (unLoc lide)
    ladefs <- mapM typeCheckDef ludefs
    (does_ret, ltstmt) <- typeCheckStmt (AType ftype) lustmt
    if (not does_ret) && ((AType ftype) /= (AType TTypeProc))
       then tcNoRetErr (unLoc lide) (getLoc ltstmt)
       else return ()
    rawCloseScopeM
    return (L loc $ ADef (TDefFunE fname (L type_loc ftype) ladefs ltstmt) ftype)

-- ---------------------------
-- Error when functions doesn't return a value
tcNoRetErr :: Ide -> SrcSpan -> TcM ()
tcNoRetErr ide loc = do
    -- we have to take the end of statements location
    let loc' = srcLocSpan (srcSpanEnd loc)
    addNoRetError loc' ide ""


-- -------------------------------------------------------------------
-- TypeCheck UStmt

typeCheckStmt :: AType -> Located UStmt -> TcM (Bool, Located TStmt)
-- UStmtNothing
typeCheckStmt ret_type (L loc UStmtNothing) = do
    return (False, L loc TStmtNothing)
-- UStmtAssign
typeCheckStmt ret_type lustmt@(L loc (UStmtAssign luvar luexpr)) = do
    lavar@(L locv (AVariable tvar var_type)) <- typeCheckVariable luvar
    laexpr@(L loce (AExpr texpr expr_type))  <- typeCheckExpr luexpr
    if (AType var_type) == (AType TTypeUnknown) || (AType expr_type) == (AType TTypeUnknown)
       then return (False, L loc TStmtNothing)
       else do
           case test var_type expr_type of
                Just Eq -> do
                    return (False, L loc $ TStmtAssign lavar laexpr)
                Nothing -> do
                    tcAssignErr lustmt (AType var_type) (AType expr_type)
                    return (False, L loc TStmtNothing)
-- UStmtCompound
typeCheckStmt ret_type (L loc (UStmtCompound lustmts)) = do
    (does_ret, ltstmts) <- tcCompoundStmt loc ret_type lustmts
    return (does_ret, L loc $ TStmtCompound ltstmts)
-- UStmtFun
typeCheckStmt ret_type (L loc (UStmtFun f)) = do
    (L _ afunc) <- typeCheckFunc (L loc f)
    return (False, L loc $ TStmtFun afunc)
-- UStmtIf
typeCheckStmt ret_type (L loc (UStmtIf lucond lustmt1 m_lustmt2)) = do
    ltcond <- typeCheckCond lucond
    (dsr1, ltstmt1) <- typeCheckStmt ret_type lustmt1
    case m_lustmt2 of
         Nothing ->
             return (False, L loc $ TStmtIf ltcond ltstmt1 Nothing)
         Just lustmt2 -> do
             (dsr2, ltstmt2) <- typeCheckStmt ret_type lustmt2
             return (dsr1 && dsr2, L loc $ TStmtIf ltcond ltstmt1 (Just ltstmt2))
-- UStmtWhile
typeCheckStmt ret_type (L loc (UStmtWhile lucond lustmt)) = do
    ltcond <- typeCheckCond lucond
    (does_ret, ltstmt) <- typeCheckStmt ret_type lustmt
    return (False, L loc $ TStmtWhile ltcond ltstmt)
-- UStmtReturn
typeCheckStmt ret_type lustmt@(L loc (UStmtReturn m_expr)) = do
    case m_expr of
         Nothing -> do
             if ret_type /= (AType TTypeProc)
                then tcRetStmtErr lustmt ret_type (AType TTypeProc)
                else return ()
             return (True, L loc $ TStmtReturn Nothing)
         Just luexpr -> do
             laexpr@(L loce (AExpr texpr expr_type)) <- typeCheckExpr luexpr
             if ret_type /= (AType expr_type)
                then tcRetStmtErr lustmt ret_type (AType expr_type)
                else return ()
             return (True, L loc $ TStmtReturn (Just laexpr))

-- ---------------------------
-- Type Check compound stmts
-- As first argument (SrcSpan) we have to srcspan of the compound stmt
-- As second argument (AType) we have the return type of the block
tcCompoundStmt :: SrcSpan -> AType -> [Located UStmt] -> TcM (Bool, [Located TStmt])
tcCompoundStmt _ _ [] = do
    return (False, [])
tcCompoundStmt loc ret_type (lustmt:lustmts) = do
    (r1, ltstmt)  <- typeCheckStmt ret_type lustmt
    if not r1
       then do
           (r2, ltstmts) <- tcCompoundStmt loc ret_type lustmts
           return (r2, ltstmt:ltstmts)
       else do
           if null lustmts
              then do
                  return (True, [ltstmt])
              else do
                  let unreach_start = srcSpanStart (getLoc (head lustmts))
                      unreach_end   = srcSpanEnd loc
                      unreach_loc   = mkSrcSpan unreach_start unreach_end
                  tcUnreachableErr unreach_loc
                  return (True, [ltstmt])

-- ---------------------------
-- Error when the types of expression and variable in an assigment are different
tcAssignErr :: Located UStmt -> AType -> AType -> TcM ()
tcAssignErr (L loc ustmt@(UStmtAssign _ _)) vtype etype =
    addTypeError loc (UAstS ustmt)
        ("Lvalue is of type `" ++ show vtype ++ "' but Rvalue is of type `" ++
         show etype ++ "'")

-- Error when we have unreachable code on a block
tcUnreachableErr :: SrcSpan -> TcM ()
tcUnreachableErr loc =
    addUnreachWarning loc
        ("Dead code has been eliminated")

-- Error when the return type is different from the one in function header
tcRetStmtErr :: Located UStmt -> AType -> AType -> TcM ()
tcRetStmtErr (L loc ustmt@(UStmtReturn _)) exptype acttype = do
    fun_name <- getNameM
    addTypeError loc (UAstS ustmt)
        ("Incopatible return type of function `" ++  show fun_name ++
         "'\n\tExpected `" ++ show exptype ++
         "' but instead function returned `" ++ show acttype ++ "'")


-- -------------------------------------------------------------------
-- TypeCheck UExpr

typeCheckExpr :: Located UExpr -> TcM (Located AExpr)
-- UExprInt
typeCheckExpr (L loc (UExprInt i)) = do
    return (L loc $ AExpr (TExprInt (fromIntegral i)) (TTypeInt))
-- UExprChar
typeCheckExpr (L loc (UExprChar c)) = do
    return (L loc $ AExpr (TExprChar (toEnum (fromEnum c))) (TTypeChar))
-- UExprString
typeCheckExpr (L loc (UExprString s)) = do
    return (L loc $ AExpr (TExprString s) (TTypeArray (length s) TTypeChar))
-- UExprVar
typeCheckExpr (L loc (UExprVar v)) = do
    (L _ (AVariable tvar ttype)) <- typeCheckVariable (L loc v)
    return (L loc $ AExpr (TExprVar tvar) ttype)
-- UExprFun
typeCheckExpr(L loc (UExprFun f)) = do
    (L _ (AFuncCall tfun ttype)) <- typeCheckFunc (L loc f)
    return (L loc $ AExpr (TExprFun tfun) ttype)
-- UExprMinus
typeCheckExpr (L loc (UExprMinus luexpr)) = do
    (L teloc (AExpr texpr ttype)) <- typeCheckExpr luexpr
    return (L loc $ AExpr (TExprMinus (L teloc texpr)) ttype)
-- UExprOp
typeCheckExpr luexpr@(L loc (UExprOp lue1 lop lue2)) = do
    (L l1 (AExpr te1 tt1)) <- typeCheckExpr lue1
    (L l2 (AExpr te2 tt2)) <- typeCheckExpr lue2
    let lte1 = L l1 te1
        lte2 = L l2 te2
        unknown_expr = (TExprVar (TVar "unknown" TTypeUnknown))
    if (AType tt1) == (AType TTypeUnknown) || (AType tt2) == (AType TTypeUnknown)
       then return (L loc $ AExpr unknown_expr TTypeUnknown)
       else do
           case test tt1 tt2 of
                Just Eq -> do
                    return (L loc $ AExpr (TExprOp lte1 lop lte2) tt1)
                Nothing -> do
                    tcOpExprErr luexpr (AType tt1) (AType tt2)
                    return (L loc $ AExpr unknown_expr TTypeUnknown)

-- ---------------------------
-- Error when the types of expressions on TExprOp are different
tcOpExprErr :: Located UExpr -> AType -> AType -> TcM ()
tcOpExprErr (L loc uexpr@(UExprOp _ lop _)) ftype stype =
    addTypeError loc (UAstE uexpr)
        ("First argument of `" ++ show (unLoc lop) ++ "' is of type `" ++
         show ftype ++ "'\n\tSecond argument of `" ++ show (unLoc lop) ++
         "' is of type `" ++ show stype ++ "'")


-- -------------------------------------------------------------------
-- TypeCheck UCond

typeCheckCond :: Located UCond -> TcM (Located TCond)
-- UCondTrue
typeCheckCond (L loc UCondTrue) = do
    return (L loc TCondTrue)
-- UCondFalse
typeCheckCond (L loc UCondFalse) = do
    return (L loc TCondFalse)
-- UCondNot
typeCheckCond (L loc (UCondNot lucond)) = do
    ltcond <- typeCheckCond lucond
    return (L loc (TCondNot ltcond))
typeCheckCond lucond@(L loc (UCondOp lue1 lop lue2)) = do
    lae1@(L l1 (AExpr te1 tt1)) <- typeCheckExpr lue1
    lae2@(L l2 (AExpr te2 tt2)) <- typeCheckExpr lue2
    if (AType tt1) == (AType TTypeUnknown) || (AType tt2) == (AType TTypeUnknown)
       then return (L loc TCondFalse)
       else do
           case test tt1 tt2 of
                Just Eq -> do
                    return (L loc $ TCondOp lae1 lop lae2)
                Nothing -> do
                    tcOpCondErr lucond (AType tt1) (AType tt2)
                    return (L loc TCondFalse)
-- UCondLog
typeCheckCond (L loc (UCondLog luc1 lop luc2)) = do
    ltc1 <- typeCheckCond luc1
    ltc2 <- typeCheckCond luc2
    return (L loc $ TCondLog ltc1 lop ltc2)

-- ---------------------------
-- Error when the types of expressions on TCondOp are different
tcOpCondErr :: Located UCond -> AType -> AType -> TcM ()
tcOpCondErr (L loc ucond@(UCondOp _ lop _)) ftype stype =
    addTypeError loc (UAstC ucond)
        ("First argument of `" ++ show (unLoc lop) ++ "' is of type `" ++
         show ftype ++ "'\n\tSecond argument of `" ++ show (unLoc lop) ++
         "' is of type `" ++ show stype ++ "'")


-- -------------------------------------------------------------------
-- TypeCheck UVariable

typeCheckVariable :: Located UVariable -> TcM (Located AVariable)
-- UVar
typeCheckVariable (L loc (UVar ide)) = do
    m_var_info <- getVarM (L loc ide)
    ide' <- getVarNameM m_var_info
    (AType var_type) <- getVarTypeM m_var_info
    return (L loc $ AVariable (TVar ide var_type) var_type)
-- UVarArray
typeCheckVariable luvar@(L loc (UVarArray lide lexpr)) = do
    (L aeloc (AExpr texpr expr_type)) <- typeCheckExpr lexpr
    m_var_info <- getVarM lide
    lide' <- liftM (L (getLoc lide)) (getVarNameM m_var_info)
    (AType var_type) <- getVarTypeM m_var_info
    let exprIsInt     = (AType expr_type) == (AType TTypeInt)
        exprIsUnknown = (AType expr_type) == (AType TTypeUnknown)
        varIsArray    = atypeIsArray (AType var_type)
        varIsUnknown  = (AType var_type)  == (AType TTypeUnknown)
    if (not exprIsInt) && (not exprIsUnknown)
       then tcIntExprErr luvar
       else return ()
    if (not varIsArray) && (not varIsUnknown)
       then tcArrayVarErr luvar (AType var_type)
       else return ()
    if exprIsInt && varIsArray
       then do
           let lexpr' = L aeloc texpr
           case test expr_type TTypeInt of
                Just Eq ->
                    return (L loc $ AVariable (TVarArray lide' var_type lexpr') var_type)
                Nothing ->
                    error "in typeCheckVariable"
       else do
           return (L loc $ AVariable (TVar "unknown" TTypeUnknown) TTypeUnknown)

-- ---------------------------
-- Check if a given AType is of TTypeArray
atypeIsArray :: AType -> Bool
atypeIsArray (AType (TTypeArray _ _)) = True
atypeIsArray _ = False

-- ---------------------------
-- Error when the array index expression is not of type of int
tcIntExprErr :: Located UVariable -> TcM ()
tcIntExprErr (L loc (uvar@(UVarArray lide lexpr))) =
    addTypeError loc (UAstV uvar)
        ("Array index `" ++ show (unLoc lexpr) ++ "' has to be of type `int'")

-- Error when variable is not of type `array'
tcArrayVarErr :: Located UVariable -> AType -> TcM ()
tcArrayVarErr (L loc uvar@(UVarArray lide lexpr)) var_type =
    addTypeError loc (UAstV uvar)
        ("Incompatible type of variable `" ++ show (unLoc lide) ++
         "'\n\tExpected `array' but variable is of type `" ++
         show var_type ++ "'")


-- -------------------------------------------------------------------
-- TypeCheck UType

typeCheckType :: Located UType -> TcM (Located AType)
-- UTypeInt
typeCheckType (L loc UTypeInt) =
    return (L loc $ AType TTypeInt)
-- UTypeChar
typeCheckType (L loc UTypeChar) =
    return (L loc $ AType TTypeChar)
-- UTypeProc
typeCheckType (L loc UTypeProc) =
    return (L loc $ AType TTypeProc)
-- UTypeArray
typeCheckType (L loc (UTypeArray (l, utype))) = do
    (L _ (AType ttype)) <- typeCheckType (L noSrcSpan utype)
    return (L loc $ AType (TTypeArray l ttype))


-- -------------------------------------------------------------------
-- TypeCheck UFuncCall

typeCheckFunc :: Located UFuncCall -> TcM (Located AFuncCall)
typeCheckFunc lufunc@(L loc (UFuncCall lide lupars)) = do
    m_fun_info <- getFuncM lide
    lide' <- liftM (L (getLoc lide)) (getFuncNameM m_fun_info)
    AType ret_type <- getFuncRetTypeM m_fun_info
    apar_type <- getFuncParamsM m_fun_info
    if (AType ret_type) /= (AType TTypeUnknown)
       then do
           lapars <- tcFunPar lufunc apar_type
           return (L loc $ AFuncCall (TFuncCall lide' ret_type lapars) ret_type)
       else do
           return (L loc $ AFuncCall (TFuncCall lide' TTypeUnknown []) TTypeUnknown)

-- ---------------------------
-- Type Check function parameters
tcFunPar :: Located UFuncCall -> [AType] -> TcM [LAExpr]
tcFunPar lufunc@(L loc (UFuncCall lide lupars)) expr_atype = do
    let pars_len = length lupars
        type_len = length expr_atype
    if pars_len /= type_len
       then do
           tcParLenErr lufunc pars_len type_len
           return []
       else do
           lapars <- tcFunPar' (unLoc lide) lupars expr_atype []
           return $ reverse lapars

tcFunPar' :: Ide -> [LUExpr] -> [AType] -> [LAExpr] -> TcM [LAExpr]
tcFunPar' ide [] [] acc = return acc
tcFunPar' ide (pexpr:pexprs) (ptype:ptypes) acc = do
    (L aeloc aexpr@(AExpr texpr ttype)) <- typeCheckExpr pexpr
    if (AType ttype) == ptype
       then return ()
       else tcParTypeErr ide pexpr ((length acc) + 1) ptype (AType ttype)
    tcFunPar' ide pexprs ptypes ((L aeloc aexpr):acc)
tcFunPar' ide _  _  _   = error "in tcFunPar'"

-- ---------------------------
-- Error when the function parameter's number is different from the prototype
tcParLenErr :: Located UFuncCall -> Int -> Int -> TcM ()
tcParLenErr (L loc ufunc@(UFuncCall lide lupars)) pars_len type_len =
    addTypeError loc (UAstF ufunc)
        ("The function `" ++ show (unLoc lide) ++ "' is applied to " ++
         show pars_len ++ " parameters but its type has " ++ show type_len)

-- Error when the function parameter's type is different from the prototype
tcParTypeErr :: Ide -> Located UExpr -> Int -> AType -> AType -> TcM ()
tcParTypeErr ide (L loc uexpr) count exptype acttype =
    addTypeError loc (UAstE uexpr)
        ("Incompatible type of argument " ++ show count ++ " of function `" ++
         show ide ++"'\n\tExpected `" ++ show exptype ++
         "' but argument is of type `" ++ show acttype ++ "'")
