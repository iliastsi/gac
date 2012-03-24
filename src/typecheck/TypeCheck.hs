--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
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
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs, PatternGuards #-}
module TypeCheck (typeCheckAst) where

import UnTypedAst
import TypedAst
import TcMonad
import SrcLoc
import Outputable (panic)
import DynFlags
import ErrUtils (MsgCode(..))

import Data.Int
import Data.Word
import Data.Char
import Control.Monad


-- ---------------------------
--
typeCheckAst :: Located UAst -> TcM TAst
typeCheckAst luast = do
    adef <- typeCheckDef luast
    case adef of
         Left tast -> return tast
         Right _   -> panic "typeCheck.typeCheckAst Either had to return Left"


-- -------------------------------------------------------------------
-- TypeCheck UDef
typeCheckDef :: Located UDef -> TcM ADef
-- UDefFun
typeCheckDef (L loc (UDefFun lide luparams lutype ludefs lustmt)) = do
    aftype <- typeCheckType lutype
    -- add function and get it's name
    fname <- liftM (L (getLoc lide)) (addFuncM lide [] aftype)
    rawOpenScopeM (unLoc lide)
    -- type check all
    adef <- tcParamDef (loc, (fname,lide), aftype, ludefs, lustmt) luparams []
    return $ Left adef
-- UDefVar
typeCheckDef ludef@(L _ (UDefVar _ _)) = do
    adef <- tcVarDef ludef id
    return $ Right adef
-- UDefArr
typeCheckDef ludef@(L _ (UDefArr _ _)) = do
    adef <- tcVarDef ludef id
    return $ Right adef

-- ---------------------------
-- Type Check parameters
tcParamDef :: (SrcSpan, (LIde,LIde), AType, [Located UDef], Located UStmt)
           -> [Located UParam] -> [(AType,Mode)] -> TcM ADefFun
tcParamDef (loc, (fname,lide), AType ftype, ludefs, lustmt) [] par_types = do
    -- update parameters in symbol table
    updateFuncM $ reverse par_types
    -- type check definitions
    adefs <- mapM typeCheckDef ludefs
    -- type check statements
    (does_ret, tstmt) <- typeCheckStmt (AType ftype) lustmt
    when ((not does_ret) && ((AType ftype) /= (AType TTypeProc))) $
            tcNoRetErr loc (unLoc lide) (AType ftype)
    rawCloseScopeM
    return $ ADefFun (TDefFun fname ftype adefs tstmt) (TTypeRetIO ftype)
tcParamDef f_info (luparam@(L _ (UParam lide mode lutype)):luparams) par_types = do
    atype@(AType ptype) <- tcArrType luparam lutype
    lide' <- liftM (L (getLoc lide)) (addVarM lide (AType ptype))
    -- type check the rest parameters
    ADefFun rest rest_type <- tcParamDef f_info luparams ((atype,mode):par_types)
    case (mode, atypeIsArray atype) of
         -- pass a non array by value
         (ModeByVal, False) -> do
             return $ ADefFun (TDefPar lide' ptype rest)
                        (TTypeFunc ptype rest_type)
         -- pass an array by value (error)
         (ModeByVal, True) -> do
             tcArrayParamErr luparam
             return $ ADefFun (TDefPar lide' ptype rest)
                        (TTypeFunc ptype rest_type)
         -- pass a non array by reference (change it to ptr)
         (ModeByRef, False) -> do
             return $ ADefFun (TDefPar lide' (TTypePtr ptype) rest)
                        (TTypeFunc (TTypePtr ptype) rest_type)
         -- pass an array by reference (normal)
         (ModeByRef, True) -> do
             return $ ADefFun (TDefPar lide' ptype rest)
                        (TTypeFunc ptype rest_type)

--  TypeCheck Array Types
tcArrType :: Located UParam -> Located UType -> TcM AType
tcArrType luparam (L _ (UTypeArr lutype lsize)) = do
    let size' = fromIntegral (unLoc lsize)
    tcCheckArraySize luparam lsize size'
    AType atype <- tcArrType luparam lutype
    return $ AType $ TTypeArray atype size'
tcArrType _ lutype = typeCheckType lutype

-- Type Check variable definitions
tcVarDef :: Located UDef -> (AType -> AType) -> TcM ADefVar
tcVarDef (L _ (UDefVar lide lutype)) type_fn = do
    AType ftype <- typeCheckType lutype
    lide' <- liftM (L (getLoc lide)) (addVarM lide (type_fn (AType ftype)))
    return $ ADefVar (TDefVar lide' ftype) ftype
tcVarDef ludef@(L _ (UDefArr luarr lsize)) type_fn = do
    let size'  = fromIntegral (unLoc lsize)
    tcCheckArraySize ludef lsize size'
    let type_fn' = type_fn . (\(AType ttype) -> AType (TTypeArray ttype size'))
    ADefVar var_def var_type <- tcVarDef luarr type_fn'
    return $ ADefVar (TDefArr var_def size') (TTypeArray var_type size')
tcVarDef _ _ = panic "TypeCheck.tcVarDef got unexpected input"

-- Check if array size is positive
tcCheckArraySize :: (Show a) => Located a -> Located Integer -> Word32 -> TcM ()
tcCheckArraySize (L ldef udef) (L loc origin) rounded = do
    flags <- getDynFlags
    let max_bound = toInteger (maxBound :: Int32)
    if origin <= 0
       then addTypeError ldef (ArrSizeError $ show udef)
              ("Array size must be a positive integer")
       else do
           when ((origin > max_bound) && dopt Opt_WarnTypeOverflows flags) $
               addTypeWarning loc (OverflowError $ show origin)
                 ("Integer has been rounded to `" ++ show rounded ++ "'")

-- ---------------------------
-- Error when functions doesn't return a value
tcNoRetErr :: SrcSpan -> Ide -> AType -> TcM ()
tcNoRetErr loc ide ftype = do
    -- we have to take the end of statements location
    addTypeError loc (NoRetError ide)
      ("Function `" ++ ide ++ "' is of type `" ++ show ftype ++ "'")

-- Error when passing an array as value
tcArrayParamErr :: Located UParam -> TcM ()
tcArrayParamErr (L loc uparam) =
    addTypeError loc (TypeError $ show uparam)
      ("Array parameters have to be passed by reference")


-- -------------------------------------------------------------------
-- TypeCheck UStmt

typeCheckStmt :: AType -> Located UStmt -> TcM (Bool, TStmt)
-- UStmtNothing
typeCheckStmt _ (L _ UStmtNothing) = do
    return (False, TStmtNothing)
-- UStmtAssign
typeCheckStmt _ lustmt@(L _ (UStmtAssign luvar luexpr)) = do
    avar@(AVariable _ var_type) <- typeCheckVariable luvar
    aexpr@(AExpr _ expr_type)  <- typeCheckExpr luexpr
    if (AType var_type) == (AType TTypeUnknown) || (AType expr_type) == (AType TTypeUnknown)
       then return (False, TStmtNothing)
       else do
           case test var_type expr_type of
                Just Eq -> do
                    return (False, TStmtAssign avar aexpr)
                Nothing -> do
                    tcAssignErr lustmt (AType var_type) (AType expr_type)
                    return (False, TStmtNothing)
-- UStmtCompound
typeCheckStmt ret_type (L _ (UStmtCompound lustmts)) = do
    (does_ret, tstmts) <- tcCompoundStmt ret_type lustmts []
    return (does_ret, TStmtCompound tstmts)
-- UStmtFun
typeCheckStmt _ (L loc (UStmtFun lf@(L _ (UFuncCall fname _)))) = do
    afunc@(AFuncCall _ ftype) <- typeCheckFunc lf
    flags <- getDynFlags
    when ((ATypeF ftype)/=(ATypeF $ TTypeRetIO TTypeProc) &&
         (ATypeF ftype)/=(ATypeF $ TTypeRetIO TTypeUnknown) &&
         (dopt Opt_WarnUnusedResult flags)) $
                addTypeWarning loc (UnusedRsError (unLoc fname)) ""
    return (False, TStmtFun afunc)
-- UStmtIf
typeCheckStmt ret_type (L _ (UStmtIf lucond lustmt1 m_lustmt2)) = do
    acond <- typeCheckCond lucond
    (dsr1, tstmt1) <- typeCheckStmt ret_type lustmt1
    case m_lustmt2 of
         Nothing ->
             return (False, TStmtIf acond tstmt1 Nothing)
         Just lustmt2 -> do
             (dsr2, tstmt2) <- typeCheckStmt ret_type lustmt2
             return (dsr1 && dsr2, TStmtIf acond tstmt1 (Just tstmt2))
-- UStmtWhile
typeCheckStmt ret_type (L _ (UStmtWhile lucond lustmt)) = do
    acond <- typeCheckCond lucond
    (_, tstmt) <- typeCheckStmt ret_type lustmt
    return (False, TStmtWhile acond tstmt)
-- UStmtReturn
typeCheckStmt ret_type lustmt@(L _ (UStmtReturn m_expr)) = do
    case m_expr of
         Nothing -> do
             when (ret_type /= (AType TTypeProc)) $
                    tcRetStmtErr lustmt ret_type (AType TTypeProc)
             return (True, TStmtReturn Nothing)
         Just luexpr -> do
             aexpr@(AExpr _ expr_type) <- typeCheckExpr luexpr
             when (ret_type /= (AType expr_type)) $
                    tcRetStmtErr lustmt ret_type (AType expr_type)
             return (True, TStmtReturn (Just aexpr))

-- ---------------------------
-- Type Check compound stmts
-- As first argument (AType) we have the return type of the block
tcCompoundStmt :: AType -> [Located UStmt] -> [TStmt] -> TcM (Bool, [TStmt])
tcCompoundStmt _ [] acc = do
    return (False, reverse acc)
tcCompoundStmt ret_type (lustmt:lustmts) acc = do
    (r1, tstmt)  <- typeCheckStmt ret_type lustmt
    if not r1
       then do
           -- r1 is False thus we didn't get any return
           tcCompoundStmt ret_type lustmts (tstmt:acc)
       else do
           if null lustmts
              then do
                  -- we get return but this is the last command
                  return (True, reverse (tstmt:acc))
              else do
                  -- we get return and we have more to do
                  -- *bang*, unreachable code
                  let unreach_start = srcSpanStart (getLoc (head lustmts))
                      unreach_end   = srcSpanEnd (getLoc (last lustmts))
                      unreach_loc   = mkSrcSpan unreach_start unreach_end
                  tcUnreachableErr unreach_loc
                  return (True, reverse (tstmt:acc))

-- ---------------------------
-- Error when the types of expression and variable in an assigment are different
tcAssignErr :: Located UStmt -> AType -> AType -> TcM ()
tcAssignErr (L loc ustmt@(UStmtAssign _ _)) vtype etype =
    addTypeError loc (TypeError $ show ustmt)
      ("Lvalue is of type `" ++ show vtype ++ "' but Rvalue is of type `" ++
       show etype ++ "'")
tcAssignErr _ _ _ = panic "TypeCheck.tcAssignErr got unexpected input"

-- Error when we have unreachable code on a block
tcUnreachableErr :: SrcSpan -> TcM ()
tcUnreachableErr loc = do
    flags <- getDynFlags
    when (dopt Opt_WarnUnreachableCode flags) $
        addTypeWarning loc UnreachError ("Dead code has been eliminated")

-- Error when the return type is different from the one in function header
tcRetStmtErr :: Located UStmt -> AType -> AType -> TcM ()
tcRetStmtErr (L loc ustmt@(UStmtReturn _)) exptype acttype = do
    fun_name <- getNameM
    addTypeError loc (TypeError $ show ustmt)
      ("Incopatible return type of function `" ++ fun_name ++
       "'\n\tExpected `" ++ show exptype ++
       "' but instead function returned `" ++ show acttype ++ "'")
tcRetStmtErr _ _ _ = panic "TypeCheck.tcRetStmtErr got unexpected input"


-- -------------------------------------------------------------------
-- TypeCheck UExpr

typeCheckExpr :: Located UExpr -> TcM AExpr
-- UExprInt
typeCheckExpr (L loc (UExprInt i)) = do
    let i' = fromIntegral i
    tcCheckIntOverflow (L loc i) i'
    return $ AExpr (TExprInt i') (TTypeInt)
-- UExprChar
typeCheckExpr (L _ (UExprChar c)) = do
    return $ AExpr (TExprChar (fromIntegral (ord c))) (TTypeChar)
-- UExprString
typeCheckExpr (L _ (UExprString s)) = do
    return $ AExpr (TExprString s) (TTypePtr TTypeChar)
-- UExprVar
typeCheckExpr (L loc (UExprVar v)) = do
    AVariable tvar ttype <- typeCheckVariable (L loc v)
    return $ AExpr (TExprVar tvar) ttype
-- UExprFun
typeCheckExpr (L loc (UExprFun f)) = do
    (AFuncCall tfun (TTypeRetIO ttype)) <- typeCheckFunc (L loc f)
    return $ AExpr (TExprFun tfun) ttype
-- UExprSign
typeCheckExpr (L loc (UExprSign (L _ OpMinus) (L _ (UExprInt i)))) = do
    typeCheckExpr (L loc (UExprInt (-i)))
typeCheckExpr luexpr@(L _ (UExprSign lop lue1)) = do
    aexpr@(AExpr te1 tt1) <- typeCheckExpr lue1
    case test tt1 (TTypeInt) of
         Just Eq ->
             if (unLoc lop) == OpPlus
                then return aexpr
                else return $ AExpr (TExprMinus te1) tt1
         Nothing ->
             if (AType tt1) == (AType TTypeUnknown)
                then return aexpr
                else do
                    tcSignExprErr luexpr (AType tt1)
                    return $ AExpr unknown_expr TTypeUnknown
-- UExprParen
typeCheckExpr (L _ (UExprParen luexpr)) =
    typeCheckExpr luexpr
-- UExprOp
typeCheckExpr luexpr@(L _ (UExprOp lue1 lop lue2)) = do
    AExpr te1 tt1 <- typeCheckExpr lue1
    AExpr te2 tt2 <- typeCheckExpr lue2
    if (AType tt1) == (AType TTypeUnknown) || (AType tt2) == (AType TTypeUnknown)
       then return $ AExpr unknown_expr TTypeUnknown
       else do
           int_or_byte <- isIntOrByte luexpr (unLoc lue1) (AType tt1) (unLoc lue2) (AType tt2)
           if int_or_byte
              then do
                  case (test tt1 tt2, test tt1 TTypeInt, test tt1 TTypeChar) of
                       (Just Eq, Just Eq, Nothing) -> do
                           return $ AExpr (TExprIntOp te1 (unLoc lop) te2) tt1
                       (Just Eq, Nothing, Just Eq) -> do
                           return $ AExpr (TExprChrOp te1 (unLoc lop) te2) tt1
                       _ -> do
                           tcOpExprErr luexpr (AType tt1) (AType tt2)
                           return $ AExpr unknown_expr TTypeUnknown
              else do
                  return $ AExpr unknown_expr TTypeUnknown

-- ---------------------------
-- Check if expressions is of either type int or char
isIntOrByte :: (Show a) => Located a -> UExpr -> AType ->
                UExpr -> AType -> TcM Bool
isIntOrByte luexpr ue1 at1 ue2 at2 = do
    int_or_byte1 <- isIntOrByte' luexpr ue1 at1
    if int_or_byte1
       then isIntOrByte' luexpr ue2 at2
       else return False

isIntOrByte' :: (Show a) => Located a -> UExpr -> AType -> TcM Bool
isIntOrByte' (L loc uexpr) ue at = do
    if at == (AType TTypeInt) || at == (AType TTypeChar)
       then return True
       else do
           addTypeError loc (TypeError $ show uexpr)
             ("Expected `int' or `byte' but expression `" ++
              show ue ++ "' is of type `" ++ show at ++ "'")
           return False

-- Return an expression with unknown type
unknown_expr :: TExpr ()
unknown_expr = TExprVar unknown_var

-- Check for integer overflows
tcCheckIntOverflow :: Located Integer -> Int32 -> TcM ()
tcCheckIntOverflow (L loc origin) rounded = do
    flags <- getDynFlags
    let max_bound = toInteger (maxBound :: Int32)
        min_bound = toInteger (minBound :: Int32)
    when ((origin < min_bound || origin > max_bound) && dopt Opt_WarnTypeOverflows flags) $
            addTypeWarning loc (OverflowError $ show origin)
                ("Integer has been rounded to `" ++ show rounded ++ "'")

-- ---------------------------
-- Error when the types of expressions on TExprOp are different
tcOpExprErr :: Located UExpr -> AType -> AType -> TcM ()
tcOpExprErr (L loc uexpr@(UExprOp _ lop _)) ftype stype =
    addTypeError loc (TypeError $ show uexpr)
      ("First argument of `" ++ show (unLoc lop) ++ "' is of type `" ++
       show ftype ++ "'\n\tSecond argument of `" ++ show (unLoc lop) ++
       "' is of type `" ++ show stype ++ "'")
tcOpExprErr _ _ _ = panic "TypeCheck.tcOpExprErr got unexpected input"

-- Error when type of expression in UExprSign is different than integer
tcSignExprErr :: Located UExpr -> AType -> TcM ()
tcSignExprErr (L loc uexpr@(UExprSign _ _)) etype =
    addTypeError loc (TypeError $ show uexpr)
      ("Expected `int' but expression is of type `" ++ show etype ++ "'")
tcSignExprErr _ _ = panic "TypeCheck.tcSignExprErr got unexpected input"


-- -------------------------------------------------------------------
-- TypeCheck UCond

typeCheckCond :: Located UCond -> TcM (ACond)
-- UCondTrue
typeCheckCond (L _ UCondTrue) = do
    return $ ACond TCondTrue
-- UCondFalse
typeCheckCond (L _ UCondFalse) = do
    return $ ACond TCondFalse
-- UCondNot
typeCheckCond (L _ (UCondNot lucond)) = do
    ACond tcond <- typeCheckCond lucond
    return $ ACond (TCondNot tcond)
typeCheckCond lucond@(L _ (UCondOp lue1 lop lue2)) = do
    AExpr te1 tt1 <- typeCheckExpr lue1
    AExpr te2 tt2 <- typeCheckExpr lue2
    if (AType tt1) == (AType TTypeUnknown) || (AType tt2) == (AType TTypeUnknown)
       then return $ ACond TCondFalse
       else do
           int_or_byte <- isIntOrByte lucond (unLoc lue1) (AType tt1) (unLoc lue2) (AType tt2)
           if int_or_byte
              then do
                  case (test tt1 tt2, test tt1 TTypeInt, test tt1 TTypeChar) of
                       (Just Eq, Just Eq, Nothing) -> do
                           return $ ACond (TCondIntOp te1 (unLoc lop) te2)
                       (Just Eq, Nothing, Just Eq) -> do
                           return $ ACond (TCondChrOp te1 (unLoc lop) te2)
                       _ -> do
                           tcOpCondErr lucond (AType tt1) (AType tt2)
                           return $ ACond TCondFalse
              else do
                  return $ ACond TCondFalse
-- UCondLog
typeCheckCond (L _ (UCondLog luc1 lop luc2)) = do
    ACond tc1 <- typeCheckCond luc1
    ACond tc2 <- typeCheckCond luc2
    return $ ACond (TCondLog tc1 (unLoc lop) tc2)

-- ---------------------------
-- Error when the types of expressions on TCondOp are different
tcOpCondErr :: Located UCond -> AType -> AType -> TcM ()
tcOpCondErr (L loc ucond@(UCondOp _ lop _)) ftype stype =
    addTypeError loc (TypeError $ show ucond)
      ("First argument of `" ++ show (unLoc lop) ++ "' is of type `" ++
       show ftype ++ "'\n\tSecond argument of `" ++ show (unLoc lop) ++
       "' is of type `" ++ show stype ++ "'")
tcOpCondErr _ _ _ = panic "TypeCheck.tcOpCondErr got unexpected input"


-- -------------------------------------------------------------------
-- TypeCheck UVariable

typeCheckVariable :: Located UVariable -> TcM AVariable
-- UVar
typeCheckVariable (L loc (UVar ide)) = do
    m_var_info <- getVarM (L loc ide)
    ide' <- getVarNameM m_var_info
    (AType var_type) <- getVarTypeM m_var_info
    return $ AVariable (TVar ide' var_type) var_type
-- UVarArray
typeCheckVariable luarr@(L _ (UVarArray luvar luexpr)) = do
    AExpr texpr expr_type <- typeCheckExpr luexpr
    AVariable tvar var_type <- typeCheckVariable luvar
    let exprIsInt     = (AType expr_type) == (AType TTypeInt)
        exprIsUnknown = (AType expr_type) == (AType TTypeUnknown)
        varIsArray    = atypeIsArray (AType var_type)
        varIsUnknown  = (AType var_type)  == (AType TTypeUnknown)
    when ((not exprIsInt) && (not exprIsUnknown)) $
            tcIntExprErr luarr
    when ((not varIsArray) && (not varIsUnknown)) $
        tcArrayVarErr luarr (AType var_type)
    if exprIsInt && varIsArray
       then do
           (AType ptr_type) <- return $ getPointer (AType var_type)
           case (test expr_type TTypeInt, test var_type (TTypePtr ptr_type)) of
             (Just Eq, Just Eq) ->
                 return $ AVariable (TVarArray tvar texpr) ptr_type
             _ ->
                 panic "test in TypeCheck.typeCheckVariable had to return Eq"
       else do
           return $ AVariable unknown_var TTypeUnknown

-- ---------------------------
-- Check if a given AType is of TTypePtr
atypeIsArray :: AType -> Bool
atypeIsArray (AType (TTypePtr _))  = True
atypeIsArray (AType TTypeArray {}) = True
atypeIsArray _ = False

-- Take the pointed type of a TTypePtr
getPointer :: AType -> AType
getPointer (AType (TTypePtr p))     = (AType p)
getPointer (AType (TTypeArray p _)) = (AType p)
getPointer _ = panic "TypeCheck.getPointer got unexpected input"

-- Return a variable with unknown type
unknown_var :: TVariable ()
unknown_var = (TVar "unknown" TTypeUnknown)

-- ---------------------------
-- Error when the array index expression is not of type of int
tcIntExprErr :: Located UVariable -> TcM ()
tcIntExprErr (L loc (uvar@(UVarArray _ lexpr))) =
    addTypeError loc (TypeError $ show uvar)
      ("Array index `" ++ show (unLoc lexpr) ++ "' has to be of type `int'")
tcIntExprErr _ = panic "TypeCheck.tcIntExprErr got unexpected input"

-- Error when variable is not of type `array'
tcArrayVarErr :: Located UVariable -> AType -> TcM ()
tcArrayVarErr (L loc uarr@(UVarArray luvar _)) var_type =
    addTypeError loc (TypeError $ show uarr)
      ("Incompatible type of expression `" ++ show (unLoc luvar) ++
       "'\n\tExpected `array' but expression is of type `" ++
       show var_type ++ "'")
tcArrayVarErr _ _ = panic "TypeCheck.tcArrayVarErr got unexpected input"


-- -------------------------------------------------------------------
-- TypeCheck UType

typeCheckType :: Located UType -> TcM AType
-- UTypeInt
typeCheckType (L _ UTypeInt) =
    return $ AType TTypeInt
-- UTypeChar
typeCheckType (L _ UTypeChar) =
    return $ AType TTypeChar
-- UTypeProc
typeCheckType (L _ UTypeProc) =
    return $ AType TTypeProc
-- UTypePtr
typeCheckType (L _ (UTypePtr utype)) = do
    AType ttype <- typeCheckType (L wiredInSrcSpan utype)
    return $ AType (TTypePtr ttype)
typeCheckType _ = panic "TypeCheck.typeCheckType got unexpected input"

-- -------------------------------------------------------------------
-- TypeCheck UFuncCall

typeCheckFunc :: Located UFuncCall -> TcM AFuncCall
typeCheckFunc lufunc@(L _ (UFuncCall lide lupars)) = do
    m_fun_info <- getFuncM lide
    lide' <- liftM (L (getLoc lide)) (getFuncNameM m_fun_info)
    ret_type <- getFuncRetTypeM m_fun_info
    apar_type <- getFuncParamsM m_fun_info
    let given_len = length lupars
        expec_len = length apar_type
    if ret_type /= (AType TTypeUnknown)
       then do
           -- check parameters number
           if given_len /= expec_len
              then do
                  tcParLenErr lufunc given_len expec_len
                  return $ AFuncCall (TFuncCall lide' (TTypeRetIO TTypeUnknown))
                                (TTypeRetIO TTypeUnknown)
              else do
                  tcFunPar ((lide',lide), ret_type)
                        (reverse lupars, reverse apar_type) id given_len
       else do
           return $ AFuncCall (TFuncCall lide' (TTypeRetIO TTypeUnknown))
                        (TTypeRetIO TTypeUnknown)

-- ---------------------------
-- Type Check function parameters
tcFunPar :: ((LIde,LIde), AType) -> ([Located UExpr], [(AType,Mode)])
         -> (ATypeF -> ATypeF) -> Int -> TcM AFuncCall
tcFunPar ((fname,_), (AType ftype)) ([],[]) type_fn _ = do
    ATypeF ftype' <- return $ type_fn (ATypeF $ TTypeRetIO ftype)
    return $ AFuncCall (TFuncCall fname ftype') ftype'
tcFunPar f_info@((_,lide), _) ((lupar:lupars),((ptype,mode):ptypes)) type_fn cnt = do
    aexpr@(AExpr texpr ttype) <- typeCheckExpr lupar
    AExpr texpr' ttype' <-
        if (not ((AType ttype) == ptype || (AType ttype) == (AType TTypeUnknown)))
           then do
               tcParTypeErr (unLoc lide) lupar cnt ptype (AType ttype)
               return aexpr
           else do
               case (mode, texpr, atypeIsArray $ AType ttype) of
                    (ModeByRef, _, True) -> return aexpr
                    (ModeByRef, TExprVar tev, _) ->
                        return (AExpr (TExprVar $ TVarPtr tev) (TTypePtr ttype))
                    (ModeByRef, _, _) -> do
                        tcParRefErr (unLoc lide) lupar cnt
                        return aexpr
                    (ModeByVal, _, _) -> return aexpr
    -- type check the rest parameters
    let type_fn' = (\(ATypeF rtype) -> ATypeF (TTypeFunc ttype' rtype)) . type_fn
    AFuncCall rest (TTypeFunc ctype rtype) <-
                tcFunPar f_info (lupars,ptypes) type_fn' (cnt-1)
    case test ttype' ctype of
         Just Eq ->
             return $ AFuncCall (TParamCall texpr' rest) rtype
         Nothing ->
             panic "test in TypeCheck.tcFunPar had to return Eq"
tcFunPar _ _ _ _ = panic "TypeCheck.tcFunPar got unexpected input"


-- ---------------------------
-- Error when the function parameter's number is different from the prototype
tcParLenErr :: Located UFuncCall -> Int -> Int -> TcM ()
tcParLenErr (L loc ufunc@(UFuncCall lide _)) pars_len type_len =
    addTypeError loc (TypeError $ show ufunc)
      ("The function `" ++ (unLoc lide) ++ "' is applied to " ++
       show pars_len ++ " parameters but its type has " ++ show type_len)

-- Error when the function parameter's type is different from the prototype
tcParTypeErr :: Ide -> Located UExpr -> Int -> AType -> AType -> TcM ()
tcParTypeErr ide (L loc uexpr) count exptype acttype =
    addTypeError loc (TypeError $ show uexpr)
      ("Incompatible type of argument " ++ show count ++ " of function `" ++
       ide ++"'\n\tExpected `" ++ show exptype ++
       "' but argument is of type `" ++ show acttype ++ "'")

-- Error when passing a non-lvalue as reference
tcParRefErr :: Ide -> Located UExpr -> Int -> TcM ()
tcParRefErr ide (L loc uexpr) count =
    addTypeError loc (TypeError $ show uexpr)
      ("Incompatible type of argument " ++ show count ++ " of function `" ++
      ide ++ "'\n\tReference expected, value given")
