
module TypeChecker.Objects where

-- Functions specific for type checking classes and objects

import Control.Monad
import Control.Monad.State

import AbsJavalette
import PrintJavalette

import TypeChecker.Context
import TypeChecker.Pointers

-- Register a class
registerClass :: TopDef -> CF ()
registerClass (ClassDef id _) = registerClass' id
registerClass (ExtendDef id _ _) = registerClass' id
registerClass _ = return ()

-- (Helper for registerClass)
registerClass' :: Ident -> CF ()
registerClass' id = CF $
    modify (\env -> env { typedefOf = (id, Custom id) : typedefOf env
                        , customKind = (id, Custom) : customKind env
                        })

-- Add a class and its declarations
addClass :: TopDef -> CF ()
addClass (ClassDef id body) = do
    addClassVars id body []
    addMethods id body []
addClass (ExtendDef id super body) = do
    superVars <- getClassVars super
    superMethods <- getMethods super
    addClassVars id body superVars
    addMethods id body superMethods
    addInhertance id super
addClass _ = return ()

-- Add inhertance between two classes
addInhertance :: Ident -> Ident -> CF ()
addInhertance id super = CF $
    modify (\env -> env {inhertance = (id, super) : inhertance env})

-- Get super of this class if it exists
getSuper :: Ident -> CF (Maybe Ident)
getSuper id = CF $ do
    env <- get
    return $ lookup id (inhertance env)

-- Get all super classes of this class
getSupers :: Ident -> CF [Type]
getSupers id = do
    maybeSuper <- getSuper id
    case maybeSuper of
        Just super -> do
            supers <- getSupers super
            return (Custom id : supers)
        Nothing -> return [Custom id]

-- Add variables of a class
addClassVars :: Ident -> [ClassDecl] -> [(Ident, Type)] -> CF ()
addClassVars id ((ClassVar vardecl) : rest) vars = do
    elems <- splitDecl vardecl
    case checkDuplicates (fst (unzip elems) ++ fst (unzip vars)) of
        Just d -> fail $ printTree d ++ " is already defined in " ++ printTree id
        Nothing -> addClassVars id rest (elems ++ vars)
addClassVars id (_ : rest) vars = addClassVars id rest vars
addClassVars id [] vars = CF $
    modify (\env -> env {classVars = (id, vars) : classVars env})

-- Add methods of a class
addMethods :: Ident -> [ClassDecl] -> [(Ident, Type)] -> CF ()
addMethods id ((Method t name args _) : rest) ms = do
    t' <- translateTypedef t
    args' <- mapM getType args
    let ft = Fun t' args'
    case lookup name ms of
        Just ot -> if ot == ft
                       then addMethods id rest ((name, ft) : ms)
                       else fail $ printTree name
                                 ++ " already defined with other type in "
                                 ++ printTree id
        Nothing -> addMethods id rest ((name, ft) : ms)
  where getType (Arg t _) = translateTypedef t
addMethods id (_ : rest) ms = addMethods id rest ms
addMethods id [] ms = CF $
    modify (\env -> env {methods = (id, ms) : methods env})

-- Lookup the type of a method
lookupMethod :: Ident -> Ident -> CF Type
lookupMethod c id = CF $ do
    cMethods <- unCF $ getMethods c
    case lookup id cMethods of
        Just mt -> return mt
        Nothing -> fail $ "Class " ++ printTree c ++
                   " does not have a method " ++ printTree id ++ " defined"

-- Get variables of a class
getClassVars :: Ident -> CF [(Ident, Type)]
getClassVars = getClassDecls classVars

-- Get methods of a class
getMethods :: Ident -> CF [(Ident, Type)]
getMethods = getClassDecls methods

-- Get a part of declarations of a class
getClassDecls :: (CFState -> [(Ident, [(Ident, Type)])]) -> Ident
              -> CF [(Ident, Type)]
getClassDecls f id = CF $ do
    env <- get
    case lookup id (f env) of
        Just content -> return content
        Nothing -> fail $ "Class " ++ printTree id ++ " is undeclared"

-- Enter a class scope
classScope :: Ident -> CF a -> CF a
classScope id m = CF $ do
    modify (\env -> env {inClass = Just id})
    a <- unCF $ scope m
    modify (\env -> env {inClass = Nothing})
    return a

-- Get the class the code is currently in
isInClass :: CF (Maybe Ident)
isInClass = CF $ do
    env <- get
    return (inClass env)
