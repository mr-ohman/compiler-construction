
module Compiler.Pointers where

-- Functions specific for compiling structures and pointers

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import AbsJavalette

import Compiler.Context

import Utils


-- Prepares a structure and its type
prepareStruct :: TopDef -> CF ()
prepareStruct (StructDef id decls) = CF $ do
    env <- get
    let elems = splitDecls decls
        (elemIds, elemTs) = unzip elems
        nativeStruct = nativeStructName id +-+ "=" +-+ nativeStructType elemTs
    put $ env { structs = (id, numberElements 0 elemIds) : structs env
              , types = nativeStruct : types env
              , sizes = (Pointer id, combinedSize elemTs) : sizes env
              }
prepareStruct _ = return ()

-- Get the LLVM name of a structure
nativeStructName :: Ident -> Instruction
nativeStructName (Ident name) = "%_struct" ++ name

-- The LLVM type for a structure
nativeStructType :: [Type] -> Instruction
nativeStructType ts = "type {" ++ seperate ", " (map nativeType ts) ++ "}"

-- Get the memory space a structure needs
getStructSize :: Type -> CF Instruction
getStructSize t = CF $ do
    env <- get
    case lookup t (sizes env) of
        Just size -> return (show size)
        Nothing -> error "getStructSize: No such struct"

-- Get the position instruction for a structure element
lookupStruct :: Type -> Ident -> CF Instruction
lookupStruct (Pointer struct) id = lookupStruct' struct id
lookupStruct (Custom struct) id = lookupStruct' struct id

-- (Helper for lookupStruct)
lookupStruct' :: Ident -> Ident -> CF Instruction
lookupStruct' struct id = CF $ do
    env <- get
    case lookup struct (structs env) of
        Just idMap -> case lookup id idMap of
            Just instr -> return instr
            Nothing -> error "lookupStruct: No such Ident in struct"
        Nothing -> error "lookupStruct: No such struct"

-- Get an element of a structure at a position
structPosition :: Type -> Instruction -> Instruction -> CF Instruction
structPosition t struct pos = structureElement t struct [pos]
