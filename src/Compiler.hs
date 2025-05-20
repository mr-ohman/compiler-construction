
module Compiler (compile) where

-- Code generator for the Javalette language

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Data.List (partition)

import AbsJavalette
import PrintJavalette

import Utils

import Compiler.Context
import Compiler.Arrays
import Compiler.Pointers
import Compiler.Objects

-- Header of all programs
header :: [String]
header = [ "declare void @_f_printInt(i32)"
         , "declare void @_f_printDouble(double)"
         , "declare void @_f_printString(i8*)"
         , "declare i32 @_f_readInt()"
         , "declare double @_f_readDouble()"
         , "declare i8* @calloc(i32, i32)"
         , "declare i8* @multiArray([0 x i32]*, i32, i32, i32)"
         ]

--------------------------------------------------------------------------------
-- Main compilation functions
--------------------------------------------------------------------------------

-- Generates code for the program as a string
compile :: Program -> IO String
compile prog = do env <- execStateT (unCF $ compileProgram prog) initState
                  return $ unlines (reverse (constants env) ++ [""] ++
                                    header ++ [""] ++
                                    reverse (types env) ++ [""] ++
                                    reverse (map snd (arrTypes env)) ++ [""] ++
                                    reverse (functions env)
                                   )

-- Compiles a program
compileProgram :: Program -> CF ()
compileProgram (Program ds) = do
    mapM_ prepareStruct ds
    mapM_ prepareClass ds
    mapM_ compileDef ds

-- Compiles a definition
compileDef :: TopDef -> CF ()
compileDef (FnDef t id args block) = do
    newFunction t id args $
        do mapM_ compileArg args
           compileBlock block
           case t of
               Void -> retVoid
               _ -> do lwe <- labelHasExit
                       unless lwe unreachable
compileDef (StructDef id decls) = return ()
compileDef (ClassDef id body) = do
    mapM_ (compileClassDecl id) body
compileDef (ExtendDef id _ body) = do
    mapM_ (compileClassDecl id) body

-- Compiles a class declaration
compileClassDecl :: Ident -> ClassDecl -> CF ()
compileClassDecl c (Method t id args block) = do
    compileDef (FnDef t (Ident $ methodFunction c id) args block)
compileClassDecl _ _ = return ()

-- Compiles a block
compileBlock :: Block -> CF ()
compileBlock (Block stmts) = mapM_ compileStmt stmts 

-- Compiles a statement
compileStmt :: Stmt -> CF ()
compileStmt stmt = case stmt of
    Empty -> return ()
    BStmt block -> scope $ compileBlock block
    Decl t items -> mapM_ (compileItem t) items
    Ass var expr -> do reg <- compileExpr expr
                       storeVariable (getType expr) var reg
    Ret expr -> do reg <- compileExpr expr
                   ret (getType expr) reg
    VRet -> retVoid
    Cond expr stmt -> do reg <- compileExpr expr
                         lThen <- newLabel
                         lEnd  <- newLabel
                         branch reg lThen lEnd
                         newBlock lThen
                         compileStmt stmt
                         jump lEnd
                         newBlock lEnd
    CondElse expr tStmt fStmt -> do reg <- compileExpr expr
                                    lThen <- newLabel
                                    lElse <- newLabel
                                    lEnd  <- newLabel
                                    branch reg lThen lElse
                                    newBlock lThen
                                    compileStmt tStmt
                                    jump lEnd
                                    newBlock lElse
                                    compileStmt fStmt
                                    jump lEnd
                                    newBlock lEnd
    While expr stmt -> do lCheck <- newLabel
                          lLoop  <- newLabel
                          lEnd   <- newLabel
                          jump lCheck
                          newBlock lCheck
                          reg <- compileExpr expr
                          branch reg lLoop lEnd
                          newBlock lLoop
                          compileStmt stmt
                          jump lCheck
                          newBlock lEnd
    For t id expr stmt -> scope $ do
        reg <- compileExpr expr
        len <- arrayLength (Array t) reg
        loops <- load Int len
        i <- allocate Int
        store Int i "0" 
        newVar t id
        lCheck <- newLabel
        lBody  <- newLabel
        lEnd   <- newLabel
        jump lCheck
        newBlock lCheck
        i' <- load Int i
        cond <- relOp Int LTH i' loops
        branch cond lBody lEnd
        newBlock lBody
        elem <- getArrayElement (Array t) reg [i']
        elem' <- load t elem
        storeVariable t (VIdent id) elem'
        compileStmt stmt
        i'' <- addOp Int Plus i' "1"
        store Int i i''
        jump lCheck
        newBlock lEnd
    SExp expr -> compileVoidExpr expr

-- Compiles an expression
compileExpr :: Expr -> CF Instruction
compileExpr (Ann t expr) = case expr of
    EVar var -> loadVariable t var
    ELitInt n -> return (show n)
    ELitDoub n -> return (show n)
    ELitTrue -> return "true"
    ELitFalse -> return "false"
    ENew id -> do size <- getStructSize (Pointer id)
                  reg <- calloc size
                  pointerCast reg t
    ENewClass id -> do size <- getStructSize (Custom id)
                       reg <- calloc size
                       reg' <- pointerCast reg t
                       callNoReg Void (classConstructor id) [(Custom id, reg')]
                       return reg'
    ENewArr t as -> do mapM_ (addArrayType t) [1..(length as)]
                       allocateArray t as
    ENull _ -> return "null"
    EApp id exprs -> do regs <- mapM compileExpr exprs
                        let tregs = zip (map getType exprs) regs
                        call t (functionName id) tregs
    EString str -> do let len = length str
                      reg <- newString str (len + 1)
                      toGeneralSize reg (len + 1)
    EDot expr ELength -> do
        reg <- compileExpr expr
        len <- arrayLength (getType expr) reg
        load Int len
    EDot expr (EApp id exprs) -> do
        (method, args) <- methodCall t expr id exprs
        call t method args
    Neg expr -> do reg <- compileExpr expr
                   negOp t reg
    Not expr -> do reg <- compileExpr expr
                   negOp t reg
    EMul expr1 op expr2 -> do reg1 <- compileExpr expr1
                              reg2 <- compileExpr expr2
                              mulOp t op reg1 reg2
    EAdd expr1 op expr2 -> do reg1 <- compileExpr expr1
                              reg2 <- compileExpr expr2
                              addOp t op reg1 reg2
    ERel expr1 op expr2 -> do reg1 <- compileExpr expr1
                              reg2 <- compileExpr expr2
                              relOp (getType expr1) op reg1 reg2
    EAnd expr1 expr2 -> lazyOp True expr1 expr2
    EOr expr1 expr2 -> lazyOp False expr1 expr2
    ObjCast ct expr -> do reg <- compileExpr expr
                          objectCast reg (getType expr) ct

-- Compiles an expression that returns void
compileVoidExpr :: Expr -> CF ()
compileVoidExpr (Ann t expr) = case expr of
    EApp id exprs -> do regs <- mapM compileExpr exprs
                        let tregs = zip (map getType exprs) regs
                        callNoReg t (functionName id) tregs
    EDot expr (EApp id exprs) -> do
        (method, args) <- methodCall t expr id exprs
        callNoReg t method args

--------------------------------------------------------------------------------
-- Minor and helping compilation functions
--------------------------------------------------------------------------------

-- Compiles an argument
compileArg :: Arg -> CF ()
compileArg (Arg t id@(Ident name)) = do
    newVar t id
    storeVariable t (VIdent id) (nativeArg name)

-- Compiles an assignment item
compileItem :: Type -> Item -> CF ()
compileItem t (Init id expr) = do
    reg <- compileExpr expr
    newVar t id
    storeVariable t (VIdent id) reg

-- Allocate space for an array
allocateArray :: Type -> [ArrayIndex] -> CF Instruction
allocateArray t ((AIndex expr):[]) = do
    reg <- compileExpr expr
    size1 <- mulOp Int Times reg (show (typeSize t))
    size2 <- addOp Int Plus size1 "4"
    arr <- calloc size2
    arr' <- pointerCast arr (Array t)
    len <- arrayLength (Array t) arr'
    store Int len reg
    return arr'
allocateArray t as = do
    regs <- mapM (compileExpr . innerExpr) as
    multiArray t regs

-- Calls a method for a class
methodCall :: Type -> Expr -> Ident -> [Expr]
           -> CF (Instruction, [(Type, Instruction)])
methodCall t expr id exprs = do
    reg <- compileExpr expr
    regs <- mapM compileExpr exprs
    let ts = map getType (expr : exprs) 
        tregs = zip ts (reg : regs)
    m <- loadMethod (getType expr) id reg
    m' <- pointerCast m (Fun t ts)
    return (m', tregs)

-- The negation operation
negOp :: Type -> Instruction -> CF Instruction
negOp t reg = case t of
    Doub -> emitAssign $ "fsub" +-+ nativeType t +-+ "0.0," +-+ reg
    Int  -> emitAssign $ "sub" +-+ nativeType t +-+ "0," +-+ reg
    Bool -> emitAssign $ "sub" +-+ nativeType t +-+ "true," +-+ reg

-- Operation for addition like operators
addOp :: Type -> AddOp -> Instruction -> Instruction -> CF Instruction
addOp t op reg1 reg2 = case t of
    Doub -> case op of
        Plus  -> binaryOp t "fadd" reg1 reg2
        Minus -> binaryOp t "fsub" reg1 reg2
    Int -> case op of
        Plus  -> binaryOp t "add" reg1 reg2
        Minus -> binaryOp t "sub" reg1 reg2

-- Operation for multiplication like operators
mulOp :: Type -> MulOp -> Instruction -> Instruction -> CF Instruction
mulOp t op reg1 reg2 = case t of
    Doub -> case op of
        Times -> binaryOp t "fmul" reg1 reg2
        Div   -> binaryOp t "fdiv" reg1 reg2
    Int -> case op of
        Times -> binaryOp t "mul" reg1 reg2
        Div   -> binaryOp t "sdiv" reg1 reg2
        Mod   -> binaryOp t "srem" reg1 reg2

-- Operation for relation operators
relOp :: Type -> RelOp -> Instruction -> Instruction -> CF Instruction
relOp t op reg1 reg2 = case t of
    Doub -> case op of
        LTH -> binaryOp t "fcmp olt" reg1 reg2
        LE  -> binaryOp t "fcmp ole" reg1 reg2
        GTH -> binaryOp t "fcmp ogt" reg1 reg2
        GE  -> binaryOp t "fcmp oge" reg1 reg2
        EQU -> binaryOp t "fcmp oeq" reg1 reg2
        NE  -> binaryOp t "fcmp one" reg1 reg2
    _ -> case op of
        LTH -> binaryOp t "icmp slt" reg1 reg2
        LE  -> binaryOp t "icmp sle" reg1 reg2
        GTH -> binaryOp t "icmp sgt" reg1 reg2
        GE  -> binaryOp t "icmp sge" reg1 reg2
        EQU -> binaryOp t "icmp eq" reg1 reg2
        NE  -> binaryOp t "icmp ne" reg1 reg2

-- Compiles a lazy operation of two expressions,
-- the boolean signals for which value will continue evaluation
lazyOp :: Bool -> Expr -> Expr -> CF Instruction
lazyOp cond expr1 expr2 = do
    reg1 <- compileExpr expr1
    lCont <- newLabel
    lSkip <- newLabel
    res <- allocate Bool
    store Bool res reg1
    if cond
        then branch reg1 lCont lSkip
        else branch reg1 lSkip lCont
    newBlock lCont
    reg2 <- compileExpr expr2
    store Bool res reg2
    jump lSkip
    newBlock lSkip
    reg3 <- load Bool res
    return reg3

-- Loads a value from a variable
loadVariable :: Type -> Variable -> CF Instruction
loadVariable = loadVariable' Nothing

-- Loads a value from a variable which might be in a structure
loadVariable' :: Maybe (Instruction, Type) -> Type -> Variable -> CF Instruction
loadVariable' struct t var = case var of
    VIdent id -> do vreg <- getVariable struct id
                    load t vreg
    VIndex id as -> do vreg <- getVariable struct id
                       let t' = arrayLevel t (length as)
                       vreg' <- load t' vreg
                       regs <- mapM (compileExpr . innerExpr) as
                       elem <- getArrayElement t' vreg' regs
                       load t elem
    VAnnDeref t1 v1 v2 -> do vreg <- loadVariable' struct t1 v1
                             loadVariable' (Just (vreg, t1)) t v2

-- Stores a value to a variable
storeVariable :: Type -> Variable -> Instruction -> CF ()
storeVariable = storeVariable' Nothing

-- Stores a value to a variable which might be in a structure
storeVariable' :: Maybe (Instruction, Type) -> Type -> Variable
               -> Instruction -> CF ()
storeVariable' struct t var reg = case var of
    VIdent id -> do vreg <- getVariable struct id
                    store t vreg reg
    VIndex id as -> do vreg <- getVariable struct id
                       let t' = arrayLevel t (length as)
                       vreg' <- load t' vreg
                       regs <- mapM (compileExpr . innerExpr) as
                       elem <- getArrayElement t' vreg' regs
                       store t elem reg
    VAnnDeref t1 v1 v2 -> do vreg <- loadVariable' struct t1 v1
                             storeVariable' (Just (vreg, t1)) t v2 reg

-- Get a variable instruction which might be in a structure
getVariable :: Maybe (Instruction, Type) -> Ident -> CF Instruction
getVariable struct id = case struct of
                            Just (sreg, st) -> do
                                num <- lookupStruct st id
                                structPosition st sreg num
                            Nothing -> lookupVar id
