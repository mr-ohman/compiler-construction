
module Compiler.Objects where

-- Functions specific for compiling classes and objects

import Data.List (partition)

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import AbsJavalette

import Compiler.Context

import Utils


-- Prepares a class, its methods and its variables
prepareClass (ClassDef id body) = CF $ do
    env <- get
    let cVars = splitDecls (getClassVars body)
        (cVarIds, cVarTs) = unzip cVars
        cMethods = makeMethods (getMethodSigs id body) []
        nativeClass = nativeClassName id +-+ "=" +-+ nativeClassType cVarTs
    modify (\env -> env { structs = (id, numberElements 1 cVarIds) : structs env
                        , methods = (id, cMethods) : methods env
                        , classVars = (id, cVars) : classVars env
                        , types = nativeClass : types env
                        , sizes = (Custom id, combinedSize cVarTs + pointerSize)
                                  : sizes env
                        })
    unCF $ makeConstructor id
prepareClass (ExtendDef id super body) = CF $ do
    env <- get
    let (Just superVars) = lookup super (classVars env)
        (Just superMethods) = lookup super (methods env)
        (Just superSize) = lookup (Custom super) (sizes env)
        cVars = superVars ++ splitDecls (getClassVars body)
        (cVarIds, cVarTs) = unzip cVars
        cMethods = makeMethods (getMethodSigs id body) superMethods
        nativeClass = nativeClassName id +-+ "=" +-+ nativeClassType cVarTs
    modify (\env -> env { structs = (id, numberElements 1 cVarIds) : structs env
                        , methods = (id, cMethods) : methods env
                        , classVars = (id, cVars) : classVars env
                        , types = nativeClass : types env
                        , sizes = (Custom id, combinedSize cVarTs + pointerSize)
                                  : sizes env
                        })
    unCF $ makeConstructor id
prepareClass _ = return ()

-- Get the LLVM name of this class
nativeClassName :: Ident -> Instruction
nativeClassName (Ident name) = "%_class" ++ name

-- Get the LLVM type definition of this class
nativeClassType :: [Type] -> Instruction
nativeClassType ts =
    "type {" ++ seperate ", " (["[0 x i8*]*"] ++ map nativeType ts) ++ "}"

-- Make the method entries for a class
makeMethods :: [(Ident, (Instruction, Type))] -> [MethodEntry] -> [MethodEntry]
makeMethods new old =
    let (new', old') = override new old []
    in old' ++ makeMethodEntries new' (length old')

-- Override methods with the same name
override :: [(Ident, (Instruction, Type))] -> [MethodEntry] -> [MethodEntry]
         -> ([(Ident, (Instruction, Type))], [MethodEntry])
override new (o:old) oldbuf =
    case partition (\x -> fst x == fst o) new of
        ([], new') -> override new' old (o:oldbuf)
        ([n], new') -> let (_, (nm, nt)) = n
                           (oid, (on, _, _)) = o
                           no = (oid, (on, nm, nt))
                      in override new' old (no:oldbuf)
override new [] old = (new, old)

-- Make a list of methods into a numbered method entry list
makeMethodEntries :: [(Ident, (Instruction, Type))] -> Int -> [MethodEntry]
makeMethodEntries ((id, (func, t)):ms) n =
    (id, (show n, func, t)) : makeMethodEntries ms (n+1)
makeMethodEntries [] _ = []

-- Get the class variables of a class declaration
getClassVars :: [ClassDecl] -> [StrDecl]
getClassVars ((ClassVar decl) : rest) = decl : getClassVars rest
getClassVars (_ : rest) = getClassVars rest
getClassVars [] = []

-- Get the methods of a class declaration with its type signature
getMethodSigs :: Ident -> [ClassDecl] -> [(Ident, (Instruction, Type))]
getMethodSigs c ((Method t id args _) : rest) =
    (id, ("@_f_" ++ methodFunction c id, Fun t ts)) : getMethodSigs c rest
  where ts = map (\(Arg t _) -> t) args
getMethodSigs c (_ : rest) = getMethodSigs c rest
getMethodSigs c [] = []

-- Get the function name of a method
methodFunction :: Ident -> Ident -> Instruction
methodFunction (Ident cName) (Ident mName) =
    "_class" ++ cName ++ "_method_" ++ mName

-- Get the constructor of a class
classConstructor :: Ident -> Instruction
classConstructor (Ident name) = "@_classConstr_" ++ name

-- Make the constructor of a class
makeConstructor :: Ident -> CF ()
makeConstructor id = do
    env <- CF $ get
    let t = Custom id
        nt = nativeType t
        (Just ms) = lookup id (methods env) 
    emit $ "define void" +-+ classConstructor id ++ "(" ++ nt +-+ "%obj) {"
    emit "entry:"
    reg1 <- structureElement t "%obj" ["0"]
    reg2 <- calloc (show $ length ms * pointerSize)
    reg3 <- bitcast' reg2 "i8*" "[0 x i8*]*"
    store' "[0 x i8*]*" reg1 reg3
    mapM (addToMethodTable reg3) ms
    retVoid'
    emit "}"
    emit ""
    CF $ modify (\env -> env { nextLReg = 0 })

-- Add a method to the stored method lookup table
addToMethodTable :: Instruction -> MethodEntry -> CF ()
addToMethodTable reg (_, (num, callName, t)) = do
    pos <- structureElement' "[0 x i8*]*" reg [num]
    method <- bitcast' callName (nativeType t) "i8*"
    store' "i8*" pos method

-- Load a method of a class to call
loadMethod :: Type -> Ident -> Instruction -> CF Instruction
loadMethod (Custom c) methodName reg = do
    env <- CF $ get
    let nt = nativeType (Custom c)
        (Just classMethods) = lookup c (methods env)
        (Just (methodId, _, _)) = lookup methodName classMethods
    reg1 <- structureElement' nt reg ["0"]
    reg2 <- load' "[0 x i8*]**" reg1
    method <- structureElement' "[0 x i8*]*" reg2 [methodId]
    load' "i8**" method

-- Cast between different object types
objectCast :: Instruction -> Type -> Type -> CF Instruction
objectCast reg t1 t2 = bitcast' reg (nativeType t1) (nativeType t2)
