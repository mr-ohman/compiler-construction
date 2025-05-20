
module TypeChecker where

-- Type checker for the Javalette language

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Data.Maybe (catMaybes)

import AbsJavalette
import PrintJavalette

import TypeChecker.Context
import TypeChecker.Pointers
import TypeChecker.Objects

import Utils


-- Inital function declarations
initFunctions :: [(Ident, Type)]
initFunctions =
    [ (Ident "printInt", Fun Void [Int])
    , (Ident "printDouble", Fun Void [Doub])
    , (Ident "printString", Fun Void [String])
    , (Ident "readInt", Fun Int [])
    , (Ident "readDouble", Fun Doub [])
    ]

--------------------------------------------------------------------------------
-- Main type checking functions
--------------------------------------------------------------------------------

-- Typechecks and returns either an error or an annotated program
typecheck :: Program -> IO (Either String Program)
typecheck prog = evalStateT (runErrorT $ unCF $ checkProgram prog)
                            (initState initFunctions)

-- Typecheck a program
checkProgram :: Program -> CF Program
checkProgram (Program ds) = do
    mapM_ addTypedef ds
    mapM_ addStruct ds
    mapM_ registerClass ds
    mapM_ addClass ds
    checkForMissingStruct
    mapM_ (addFunc translateTypedef) ds
    b <- checkForMain
    unless b $ fail $ "Missing correct main definition"
    annDefs <- mapM checkDef ds
    return (Program $ catMaybes annDefs)

-- Typecheck a definition
checkDef :: TopDef -> CF (Maybe TopDef)
checkDef (FnDef t id args block) = scope $ do
    t' <- translateTypedef t
    args' <- addArgs args
    setRetType t'
    (annBlock, ret) <- existsReturn (checkBlock block)
    if ret || t' == Void
       then do setReturnExist False
               return (Just $ FnDef t' id args' annBlock)
       else fail $ "Function " ++ printTree id ++ " is missing return"
checkDef (StructDef id decls) = do
    decls' <- mapM (\(StrDecl t var) -> do t' <- translateTypedef t
                                           return (StrDecl t' var)) decls
    return (Just $ StructDef id decls')
checkDef (TypeDef _ _) = return Nothing
checkDef (ClassDef id body) = do
    annBody <- mapM (checkClassDecl id) body
    return (Just $ ClassDef id annBody)
checkDef (ExtendDef id super body) = do
    annBody <- mapM (checkClassDecl id) body
    return (Just $ ExtendDef id super annBody)

-- Typecheck a class declaration
checkClassDecl :: Ident -> ClassDecl -> CF ClassDecl
checkClassDecl c decl = case decl of
        ClassVar (StrDecl t ids) -> do
            t' <- translateTypedef t
            return (ClassVar $ StrDecl t' ids)
        Method t id args fBlock -> classScope c $ do
            let self = Arg (Custom c) (Ident "self")
            (Just (FnDef annT _ annArgs annFBlock))
                <- checkDef (FnDef t id (self : args) fBlock)
            return (Method annT id annArgs annFBlock)

-- Typecheck a block
checkBlock :: Block -> CF Block
checkBlock (Block stmts) = do
    annStmts <- mapM checkStmt stmts
    return (Block annStmts)

-- Typecheck a statement
checkStmt :: Stmt -> CF Stmt
checkStmt stmt =
    case stmt of
      Empty -> return Empty
      BStmt block -> do
          annBlock <- scope (checkBlock block)
          return (BStmt annBlock)
      Decl t items -> do
          t' <- translateTypedef t
          annItems <- mapM (checkDecl t') items
          return (Decl t' annItems)
      Ass var expr -> do
          (annVar, t) <- checkVariable var
          annExpr <- checkExpr [t] expr
          return (Ass annVar annExpr)
      Incr var -> checkStmt $ desugar Plus var
      Decr var -> checkStmt $ desugar Minus var
      Ret expr -> do
          t <- getRetType
          annExpr <- checkExpr [t] expr
          setReturnExist True
          return (Ret annExpr)
      VRet -> do
           t <- getRetType
           if t == Void
               then do setReturnExist True
                       return VRet
               else fail "Cannot return void in a non-void function"
      Cond expr stmt -> do
          annExpr <- checkExpr [Bool] expr
          (annStmt, ret) <- existsReturn (checkStmt stmt)
          when (annEq ELitTrue annExpr) $ setReturnExist ret
          return (Cond annExpr annStmt)
      CondElse expr tStmt fStmt -> do
          annExpr <- checkExpr [Bool] expr
          (annTStmt, retT) <- existsReturn (checkStmt tStmt)
          (annFStmt, retF) <- existsReturn (checkStmt fStmt)
          when (annEq ELitTrue annExpr) $ setReturnExist retT
          when (annEq ELitFalse annExpr) $ setReturnExist retF
          when (retT && retF) $ setReturnExist True
          return (CondElse annExpr annTStmt annFStmt)
      While expr stmt -> do
          annExpr <- checkExpr [Bool] expr
          (annStmt, ret) <- existsReturn (checkStmt stmt)
          when (annEq ELitTrue annExpr) $ setReturnExist ret
          return (While annExpr annStmt)
      For t id expr stmt -> do
          t' <- translateTypedef t
          annExpr <- inferExpr expr
          case getType annExpr of
              Array t2 -> if t' == t2
                  then scope $ do addVar id t'
                                  annStmt <- checkStmt stmt
                                  return (For t' id annExpr annStmt)
                  else fail $ printTree id ++ " has type " ++
                              printTree t' ++ " expected " ++ printTree t2
              _ -> fail $ printTree expr ++ " is not an array"
      SExp expr -> do
          annExpr <- checkExpr [Void] expr
          return (SExp annExpr)

-- Typecheck an expression
checkExpr :: [Type] -> Expr -> CF Expr
checkExpr ts expr = do
    annExpr <- inferExpr expr
    let t = getType annExpr
    if elem Wildcard ts || elem t ts
       then return annExpr
       else case t of
            Custom c -> do
               supers <- getSupers c
               if any (`elem` ts) supers
                  then let castT = head ts in
                       annotate (ObjCast castT annExpr) castT
                  else failure t ts
            _ -> failure t ts
    where failure t ts = fail $ printTree expr ++ " has type " ++
                                printTree t ++ " expected " ++ printOr ts

-- Returns the inferred type of an expression
inferExpr :: Expr -> CF Expr
inferExpr expr = do
    case expr of
      EVar var -> do (annVar, t) <- checkVariable var
                     annotate (EVar annVar) t
      ELitInt n -> annotate expr Int
      ELitDoub d -> annotate expr Doub
      ELitTrue -> annotate expr Bool
      ELitFalse -> annotate expr Bool
      ENew id -> do t <- getCustomKind id
                    case t of
                        Pointer _ -> annotate (ENew id) t
                        Custom _ -> annotate (ENewClass id) t
      ENewArr t as -> do annAs <- checkAIndex as
                         annotate (ENewArr t annAs) (arrayLevel t (length as))
      ENull t -> do t' <- translateTypedef t
                    annotate (ENull t') t'
      EApp id exprs -> do
          (Fun t ts) <- lookupFunc id
          annExprs <- checkArguments exprs ts (printTree expr)
          annotate (EApp id annExprs) t
      EString str -> annotate expr String
      EDot expr1 expr2 -> do
          case expr2 of
              EVar (VIdent (Ident "length")) -> do
                  annExpr1 <- inferExpr expr1
                  case getType annExpr1 of
                      Array _ -> annotate (EDot annExpr1 ELength) Int
                      _ -> fail $ printTree expr ++ " is not an array"
              EApp id exprs -> do
                  annExpr1 <- inferExpr expr1
                  case getType annExpr1 of
                      Custom c -> do
                          (Fun t ts) <- lookupMethod c id
                          annExprs <- checkArguments exprs ts (printTree expr)
                          annotate (EDot annExpr1 (EApp id annExprs)) t
                      _ -> fail $ printTree expr1 ++ " is not of class type"
              _ -> fail $ printTree expr ++ " is not a supported dereference"
      Neg expr -> do annExpr <- checkExpr [Int, Doub] expr
                     annotate (Neg annExpr) (getType annExpr)
      Not expr -> do annExpr <- checkExpr [Bool] expr
                     annotate (Not annExpr) Bool
      EMul expr1 Mod expr2 -> checkTwoExpr [Int] expr1 expr2
                              (\e1 e2 -> EMul e1 Mod e2) Nothing
      EMul expr1 op expr2 -> checkTwoExpr [Int, Doub] expr1 expr2
                             (\e1 e2 -> EMul e1 op e2) Nothing
      EAdd expr1 op expr2 -> checkTwoExpr [Int, Doub] expr1 expr2
                             (\e1 e2 -> EAdd e1 op e2) Nothing
      ERel expr1 op expr2 -> if elem op [EQU, NE]
                               then checkTwoExpr [Wildcard] expr1 expr2
                                    (\e1 e2 -> ERel e1 op e2) (Just Bool)
                               else checkTwoExpr [Int, Doub] expr1 expr2
                                    (\e1 e2 -> ERel e1 op e2) (Just Bool)
      EAnd expr1 expr2 -> checkTwoExpr [Bool] expr1 expr2 EAnd Nothing
      EOr expr1 expr2 -> checkTwoExpr [Bool] expr1 expr2 EOr Nothing

--------------------------------------------------------------------------------
-- Minor and helping checking functions
--------------------------------------------------------------------------------

-- Add the arguments of a function to a context
addArgs :: [Arg] -> CF [Arg]
addArgs args = mapM (\(Arg t id) ->
    do t' <- translateTypedef t
       addVar id t'
       return (Arg t' id)) args

-- Check a variable declaration
checkDecl :: Type -> Item -> CF Item
checkDecl t item =
    case item of
      NoInit id -> case t of
          Int -> checkDecl t (Init id (ELitInt 0))
          Doub -> checkDecl t (Init id (ELitDoub 0))
          Bool -> checkDecl t (Init id ELitFalse)
          Void -> fail "Illegal void variable declaration"
          Array _ -> checkDecl t (Init id (ENull t))
          Pointer _ -> checkDecl t (Init id (ENull t))
          Custom _ -> checkDecl t (Init id (ENull t))
      Init id expr -> do
          addVar id t
          annExpr <- checkExpr [t] expr
          return (Init id annExpr)

-- Check a variable for an annotated variable and its type
checkVariable :: Variable -> CF (Variable, Type)
checkVariable v = do
    vars <- getVars
    maybeVar <- checkVariable' vars v
    case maybeVar of
        Just var -> return var
        Nothing -> do
            maybeClass <- isInClass
            case maybeClass of
                Just c -> do
                    cvs <- getClassVars c
                    maybeVar <- checkVariable' [cvs] v
                    case maybeVar of
                        Just (var, t) -> return (VAnnDeref (Custom c)
                                                (VIdent (Ident "self")) var, t)
                        Nothing -> failure
                Nothing -> failure
  where failure = fail $ "Unknown variable " ++ printTree v

-- Check a variable in a variable space and maybe get it annotated and its type
checkVariable' :: [[(Ident, Type)]] -> Variable -> CF (Maybe (Variable, Type))
checkVariable' vMap v = case v of
    VIdent id -> do
        case lookupVar vMap id of
            Just t -> return $ Just (VIdent id, t)
            Nothing -> return Nothing
    VIndex id as -> do
        case lookupVar vMap id of
            Just t -> case removeLevel (length as) t of
                Just t' -> do annAs <- checkAIndex as
                              return $ Just (VIndex id annAs, t')
                Nothing -> fail $ printTree id ++
                           " does not have array depth corresponding " ++
                           printTree v
            Nothing -> return Nothing
    VDeref v1 v2 -> do
        maybeVar1 <- checkVariable' vMap v1
        case maybeVar1 of
            Just (v1', t1) -> case t1 of
                Pointer struct -> do
                    structMap <- getStruct struct
                    maybeVar2 <- checkVariable' [structMap] v2
                    case maybeVar2 of
                        Just (v2', t2) ->
                            return $ Just (VAnnDeref t1 v1' v2', t2)
                        Nothing -> return Nothing
                _ -> fail $ printTree v1 ++ " is not of a struct type"
            Nothing -> return Nothing

-- Check an array indexation
checkAIndex :: [ArrayIndex] -> CF [ArrayIndex]
checkAIndex as = do annExprs <- mapM ((checkExpr [Int]) . innerExpr) as
                    return (map AIndex annExprs)

-- Check arguments against types
checkArguments :: [Expr] -> [Type] -> String -> CF [Expr]
checkArguments exprs ts ecmd = do
    args <- strictZip exprs ts
                      ("Wrong amount of arguments in " ++ ecmd)
    mapM (\(e, t) -> checkExpr [t] e) args

-- Check two expressions that they have same type and returns them combined
-- with an operator and returns the same type or an specified other type
checkTwoExpr :: [Type] -> Expr -> Expr -> (Expr -> Expr -> Expr)
             -> Maybe Type -> CF Expr
checkTwoExpr ts expr1 expr2 f mt = do
    annExpr1 <- checkExpr ts expr1
    let t = getType annExpr1
    annExpr2 <- checkExpr [t] expr2
    annotate (f annExpr1 annExpr2) (maybe t id mt)

--------------------------------------------------------------------------------
-- Misc utilities
--------------------------------------------------------------------------------

-- Translates an increment/decrement statement
desugar :: AddOp -> Variable -> Stmt
desugar op var = Ass var (EAdd (EVar var) op (ELitInt 1))

-- Test if an expression is equal to the underlying expression of an annotated
annEq :: Expr -> Expr -> Bool
annEq expr1 (Ann _ expr2) = expr1 == expr2

-- Convenient function for returning and annotating an expression
annotate :: Monad m => Expr -> Type -> m Expr
annotate expr t = return (Ann t expr)
