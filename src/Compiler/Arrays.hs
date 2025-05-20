
module Compiler.Arrays where

-- Functions specific for compiling arrays

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import AbsJavalette

import Compiler.Context

import Utils


-- Compiles the call process construct a multidimensional array using
-- the internal runtime function multiArray
multiArray :: Type -> [Instruction] -> CF Instruction
multiArray t regs = do
    let size = length regs
        pretype = "[" ++ show size +-+ "x i32]"
        pretypeStar = pretype ++ "*"
    prearr <- allocate' pretype
    mapM (\(n, reg) -> do elem <- structureElement' pretypeStar prearr [show n]
                          store Int elem reg
         ) (zip [0..] regs)
    sizearr <- bitcast' prearr pretypeStar "[0 x i32]*"
    arr <- emitAssign $ callCmd' "i8*" "@multiArray"
                                 [ "[0 x i32]*" +-+ sizearr
                                 , "i32" +-+ show (size - 1)
                                 , "i32" +-+ show (typeSize t)
                                 , "i32 0"
                                 ]
    pointerCast arr (arrayLevel t size)

-- Get an element from an array
getArrayElement :: Type -> Instruction -> [Instruction] -> CF Instruction
getArrayElement t arr (reg:rest) = do
    elemP <- arrayPosition t arr reg
    case rest of
        [] -> getArrayElement t elemP rest
        _ -> do let t' = (\(Array x) -> x) t
                elem <- load t' elemP
                getArrayElement t' elem rest
getArrayElement _ reg [] = return reg

-- Prepare a new array type
addArrayType :: Type -> Int -> CF ()
addArrayType t n = CF $ do
    env <- get
    let t' = arrayLevel t n
        ts = arrTypes env
    case lookup t' ts of
        Just _ -> return ()
        Nothing -> do
            let nameHead = "%_arr" ++ show t
                innerType = if n == 1
                                then nativeType t
                                else nameHead ++ show (n-1) ++ "*"
                arrName = nameHead ++ show n
                newArr = arrName +-+
                               "= type {i32, [0 x " ++ innerType ++ "]}"
            put $ env {arrTypes = (t', newArr) : ts}

-- Compiles the retriving of an array size
arrayLength :: Type -> Instruction -> CF Instruction
arrayLength t arr = structureElement t arr ["0"]

-- Compiles the retriving of an element in an array one level deep
arrayPosition :: Type -> Instruction -> Instruction -> CF Instruction
arrayPosition t arr pos = structureElement t arr ["1", pos]
