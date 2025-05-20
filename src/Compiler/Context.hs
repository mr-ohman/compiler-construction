{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Context where

-- Context for the compilation process

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import AbsJavalette

import Utils

-- LLVM instruction type
type Instruction = String

-- Entry type for a method
type MethodEntry = (Ident, (Instruction, Instruction, Type))

-- Data type for code generation state
data CFState = CFState
  { functions  :: [Instruction] -- Functions part of code
  , methods    :: [(Ident, [MethodEntry])] -- Methods of classes
  , classVars  :: [(Ident, [(Ident, Type)])] -- Variables of classes
  , structs    :: [(Ident, [(Ident, Instruction)])] -- Structure types
  , arrTypes   :: [(Type, Instruction)] -- Type definitions for arrays
  , types      :: [Instruction] -- Type definitions for structures
  , sizes      :: [(Type, Int)] -- Sizes of structures
  , constants  :: [Instruction] -- Constants part of code
  , variables  :: [[(Ident, Instruction)]] -- Scoped variable table
  , nextLReg   :: Int -- Next local register
  , nextGReg   :: Int -- Next global register
  , nextLabel  :: Int -- Next label
  , labelWExit :: Bool -- Is the last label without futher instructions?
  , lastRet    :: Bool -- Was there a return before this line in this block?
  }

-- Initial state
initState :: CFState
initState = CFState
    { functions = []
    , methods = []
    , classVars = []
    , structs = []
    , arrTypes = []
    , types = []
    , sizes = []
    , constants = []
    , variables = []
    , nextLReg = 0
    , nextGReg = 0
    , nextLabel = 0
    , labelWExit = False
    , lastRet = False
    }

-- Monad for compilation
newtype CF a = CF { unCF :: StateT CFState IO a }
    deriving (Functor, Monad)

--------------------------------------------------------------------------------
-- CF functions
--------------------------------------------------------------------------------

-- Writes a line of code to the functions part
emit :: Instruction -> CF ()
emit str = CF $ do
    env <- get
    unless (lastRet env) $ put (env { functions = str : functions env })

-- Writes a line of code to the functions part with assignment to a register
emitAssign :: Instruction -> CF Instruction
emitAssign str = do
    reg <- newLocal
    emit $ reg +-+ "=" +-+ str
    return reg

-- Get a new local register
newLocal :: CF Instruction
newLocal = CF $ do
    env <- get
    let l = nextLReg env
    put $ env {nextLReg = l + 1}
    return ("%" ++ show l)

-- Get a new label
newLabel :: CF Instruction
newLabel = CF $ do
    env <- get
    let l = nextLabel env
    put $ env {nextLabel = l + 1}
    return ("%lbl" ++ show l)

-- Returns true if the label has an exit 
labelHasExit :: CF Bool
labelHasExit = CF $ do
    env <- get
    return (labelWExit env)

-- Allocates a register with lookup for an Ident
newVar :: Type -> Ident -> CF ()
newVar t id = do reg <- allocate t
                 CF $ modify (\env -> let (scope:rest) = variables env
                                      in env {variables = ((id, reg) : scope) : rest})

-- Lookup a variable and get its register 
lookupVar :: Ident -> CF Instruction
lookupVar id = CF $ do
    env <- get
    lookup' (variables env)
  where lookup' (scope:rest) =
            case lookup id scope of
              Nothing -> lookup' rest
              Just r -> return r

-- Run a compilation in a variable scope
scope :: CF () -> CF ()
scope m = do
    CF $ modify (\env -> env {variables = [] : variables env})
    m
    CF $ modify (\env -> env {variables = tail (variables env)})

--------------------------------------------------------------------------------
-- LLVM specific functions
--------------------------------------------------------------------------------

-- Get the LLVM type of an abstract syntax type
nativeType :: Type -> Instruction
nativeType t = case t of
    Int -> "i32"
    Doub -> "double"
    Bool -> "i1"
    Void -> "void"
    String -> "i8*"
    Array t' -> let (lt, n) = getArrayLow t' 1
                in "%_arr" ++ show lt ++ show n ++ "*"
    Pointer (Ident struct) -> "%_struct" ++ struct ++ "*"
    Custom (Ident name) -> "%_class" ++ name ++ "*"
    Fun t ts -> nativeType t ++ "(" ++ seperate ", " (map nativeType ts) ++ ")*"

-- Get the size of a type
typeSize :: Type -> Int
typeSize t = case t of
    Int -> 4
    Doub -> 8
    Bool -> 1
    Array _ -> pointerSize
    Pointer _ -> pointerSize
    Custom _ -> pointerSize
    Fun _ _ -> pointerSize

-- Size of a pointer
pointerSize :: Int
pointerSize = 8

-- Name used for arguments in LLVM
nativeArg :: String -> Instruction
nativeArg s = "%_arg_" ++ s

-- Emits header of function and runs a compilation in a scope
newFunction :: Type -> Ident -> [Arg] -> CF () -> CF ()
newFunction t id args m = do
    emit $ "define" +-+ nativeType t +-+ functionName id ++ "(" ++
          argList (map translateArg args) ++ ")" +-+ "{"
    emit "entry:"
    scope m
    CF $ modify (\env -> env { lastRet = False })
    emit "}"
    emit ""
    CF $ modify (\env -> env { nextLReg = 0
                             , nextLabel = 0 })

-- Make a list of types and registers to an argument list
argList :: [(Type, Instruction)] -> Instruction
argList args = seperate ", " (map (\(t, i) -> nativeType t +-+ i) args)

-- Add a new string to the global scope and get its register
newString :: String -> Int -> CF Instruction
newString str size = CF $ do
    env <- get
    let g = nextGReg env
        v = "@" ++ show g
        s = v +-+ "= internal constant [" ++ show size +-+
            "x i8] c\"" ++ str ++ "\\00\""
    put $ env { nextGReg = g + 1
              , constants = s : constants env
              }
    return v

-- Get a general size typed string from a specific sized
toGeneralSize :: Instruction -> Int -> CF Instruction
toGeneralSize reg size = emitAssign $ "getelementptr [" ++ show size +-+ "x i8]*" +-+ reg ++ ", i32 0, i32 0"

-- Start a new block with a label
newBlock :: Instruction -> CF ()
newBlock lbl = do CF $ modify (\env -> env { lastRet = False
                                           , labelWExit = False })
                  emit $ tail lbl ++ ":"

-- Branch with a condition to one of the blocks
branch :: Instruction -> Instruction -> Instruction -> CF ()
branch cond lbl1 lbl2 = emit $ "br i1" +-+ cond ++ ", label" +-+ lbl1 ++ ", label" +-+ lbl2

-- Jump to a block
jump :: Instruction -> CF ()
jump lbl = emit $ "br label" +-+ lbl

-- Allocates a register
allocate :: Type -> CF Instruction
allocate t = allocate' (nativeType t)

-- Allocates a register (primitive function)
allocate' :: Instruction -> CF Instruction
allocate' t = emitAssign $ "alloca" +-+ t

-- Loads a value from a variable register
load :: Type -> Instruction -> CF Instruction
load t reg = load' (nativeType t ++ "*") reg

-- Loads a value from a variable register (primitive function)
load' :: Instruction -> Instruction -> CF Instruction
load' t reg = emitAssign $ "load" +-+ t +-+ reg

-- Stores a value to a variable register
store :: Type -> Instruction -> Instruction -> CF ()
store t reg1 reg2 = store' (nativeType t) reg1 reg2

-- Stores a value to a variable register (primitive function)
store' :: Instruction -> Instruction -> Instruction -> CF ()
store' t reg1 reg2 = emit $ "store" +-+ t +-+ reg2 ++ "," +-+ t ++ "*" +-+ reg1

-- Return with a register
ret :: Type -> Instruction -> CF ()
ret t reg = do emit $ "ret" +-+ nativeType t +-+ reg
               CF $ modify (\env -> env { lastRet = True })

-- Return in a void function
retVoid :: CF ()
retVoid = do retVoid'
             CF $ modify (\env -> env { lastRet = True })

-- Return in a void function
retVoid' :: CF ()
retVoid' = emit $ "ret void"

-- Signal that a block is unreachable
unreachable :: CF ()
unreachable = emit "unreachable"

-- Use an binary operator on two registers
binaryOp :: Type -> Instruction -> Instruction -> Instruction -> CF Instruction
binaryOp t op reg1 reg2 = emitAssign $ op +-+
                          nativeType t +-+ reg1 ++ "," +-+ reg2

-- Call a function that returns a value
call :: Type -> Instruction -> [(Type, Instruction)] -> CF Instruction
call t f args = emitAssign $ callCmd t f args

-- Call a function with no return
callNoReg :: Type -> Instruction -> [(Type, Instruction)] -> CF ()
callNoReg t f args = emit $ callCmd t f args

-- The command for a call
callCmd :: Type -> Instruction -> [(Type, Instruction)] -> Instruction
callCmd t func args = "call" +-+ nativeType t +-+
                             func ++ "(" ++ argList args ++ ")"

-- The command for a call (raw LLVM version)
callCmd' :: Instruction -> Instruction -> [Instruction] -> Instruction
callCmd' t func args = "call" +-+ t +-+
                              func ++ "(" ++ seperate ", " args ++ ")"

-- Name for functions, for avoiding collision with internal runtime functions
functionName :: Ident -> Instruction
functionName id = case id of
                      (Ident "main") -> "@main"
                      (Ident name) -> "@_f_" ++ name

-- Calls the calloc internal function
calloc :: Instruction -> CF Instruction
calloc size = do
    emitAssign $ "call i8* @calloc(i32 1, i32" +-+ size ++ ")"

-- Casts a pointer from one LLVM type to another
bitcast' :: Instruction -> Instruction -> Instruction -> CF Instruction
bitcast' reg t1 t2 = emitAssign $ "bitcast" +-+ t1 +-+ reg +-+ "to" +-+ t2

-- Casts a general pointer into a specific pointer of a type
pointerCast :: Instruction -> Type -> CF Instruction
pointerCast reg t = bitcast' reg "i8*" (nativeType t)

-- Get an element of a structure
structureElement :: Type -> Instruction -> [Instruction] -> CF Instruction
structureElement t struct indexes = structureElement' (nativeType t) struct indexes

-- Get an element of a structure (raw LLVM type version)
structureElement' :: Instruction -> Instruction -> [Instruction] -> CF Instruction
structureElement' t struct indexes =
    emitAssign $ "getelementptr" +-+ seperate ","
                 ([t +-+ struct] ++ map ("i32" +-+) ("0" : indexes))

--------------------------------------------------------------------------------
-- Misc. utilities
--------------------------------------------------------------------------------

-- Translate an argument to a type and register tuple
translateArg :: Arg -> (Type, Instruction)
translateArg (Arg t (Ident name)) = (t, nativeArg name)

-- Get the lowest type and dimensions of an array
getArrayLow :: Type -> Int -> (Type, Int)
getArrayLow (Array t) n = getArrayLow t (n+1)
getArrayLow t n = (t, n)

-- Split a structure declaration into multiple ident and type pairs
splitDecl :: StrDecl -> [(Ident, Type)]
splitDecl (StrDecl t ids) = map (\id -> (id, t)) ids

-- Split multiple structure declarations
splitDecls :: [StrDecl] -> [(Ident, Type)]
splitDecls decls = concatMap splitDecl decls

-- Give each ident a unique number
numberElements :: Int -> [Ident] -> [(Ident, Instruction)]
numberElements n ids = zip ids (map show [n..])

-- Calculate the combined memory need of a list of types
combinedSize :: [Type] -> Int
combinedSize ts = sum (map typeSize ts)
