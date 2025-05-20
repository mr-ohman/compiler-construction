
module TypeChecker.Pointers where

-- Functions specific for type checking structures and pointers

import Control.Monad
import Control.Monad.State

import Data.List

import AbsJavalette
import PrintJavalette

import TypeChecker.Context

-- Add a structure to the context
addStruct :: TopDef -> CF ()
addStruct (StructDef id elems) = CF $ do
    env <- get
    let strs = structs env
    case lookup id strs of
      Nothing -> do
          elems' <- unCF $ mapM splitDecl elems
          case checkDuplicates (fst $ unzip $ concat elems') of
              Just d -> fail $ printTree d ++ " is already defined in "
                               ++ printTree id
              Nothing ->
                  put $ env { structs = (id, concat elems') : strs
                            , customKind = (id, Pointer) : customKind env
                            }
      Just _  -> fail $ "Struct " ++ printTree id ++ " already declared"
addStruct _ = return ()

-- Split multi variable declaration to single variable declarations
splitDecl :: StrDecl -> CF [(Ident, Type)]
splitDecl (StrDecl t ids) = do t' <- translateTypedef t
                               return $ map (\id -> (id, t')) ids

-- Add a type definition to the context
addTypedef :: TopDef -> CF ()
addTypedef (TypeDef struct link) = CF $ do
    env <- get
    let typedefs = typedefOf env
    case lookup link typedefs of
      Nothing -> put $ env {typedefOf = (link, Pointer struct) : typedefs}
      Just _  -> fail $ "Type name " ++ printTree link ++ " already declared"
addTypedef _ = return ()

-- Get a struct's variables
getStruct :: Ident -> CF [(Ident, Type)]
getStruct id = CF $ do
    env <- get
    case lookup id (structs env) of
        Just map -> return map
        Nothing -> fail $ "Struct " ++ printTree id ++ " is not declared"

-- Get the full type of a custom type name
getCustomKind :: Ident -> CF Type
getCustomKind id = CF $ do
    env <- get
    case lookup id (customKind env) of
        Just f -> return (f id)
        Nothing -> fail $ "Custom type " ++ printTree id ++ " is undeclared"

-- Translate a type to its corresponding type using type definitions
translateTypedef :: Type -> CF Type
translateTypedef (Custom id) = CF $ do
    env <- get
    case lookup id (typedefOf env) of
        Just t' -> return t'
        Nothing -> fail $ "Type " ++ printTree id ++ " is not declared"
translateTypedef t = return t

-- Check if a structure declaration is missing
checkForMissingStruct :: CF ()
checkForMissingStruct = CF $ do
    env <- get
    let structIds = map fst $ structs env
        typedefStrs = concatMap structTypedef (typedefOf env)
        missing = (nub typedefStrs) \\ structIds
    unless (missing == []) $ fail $ "Missing struct definition " ++
                                    printTree (head missing)
  where structTypedef (_, t) = case t of
            Custom _ -> []
            Pointer t' -> [t']

--------------------------------------------------------------------------------
-- Misc. utilities
--------------------------------------------------------------------------------

-- Check if a list contains duplicates and return the duplicate
checkDuplicates :: Eq a => [a] -> Maybe a
checkDuplicates (x:xs) = if elem x xs
                             then Just x
                             else checkDuplicates xs
checkDuplicates [] = Nothing
