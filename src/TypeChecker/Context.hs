{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker.Context where

-- Typechecking context monad and functions

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import AbsJavalette
import PrintJavalette

-- Data type for type checking state
data CFState = CFState
  { functions   :: [(Ident, Type)] -- Functions of the program
  , methods     :: [(Ident, [(Ident, Type)])] -- Methods of classes
  , structs     :: [(Ident, [(Ident, Type)])] -- Variable of structures
  , locals      :: [[(Ident, Type)]] -- Local variables in scopes
  , classVars   :: [(Ident, [(Ident, Type)])] -- Variables of classes
  , inClass     :: Maybe Ident -- Is just when code is in a class
  , inhertance  :: [(Ident, Ident)] -- Inhertance of classes
  , returnType  :: Type -- The return type of the current function
  , returnExist :: Bool -- Variable for if a return exists in current function
  , typedefOf   :: [(Ident, Type)] -- Type definitions
  , customKind  :: [(Ident, Ident -> Type)] -- The kind of a custom type
  }

-- Initial state
initState :: [(Ident, Type)] -> CFState
initState initFuncs = CFState
    { functions = initFuncs
    , methods = []
    , structs = []
    , locals = []
    , classVars = []
    , inClass = Nothing
    , inhertance = []
    , returnType = Void
    , returnExist = False
    , typedefOf = []
    , customKind = []
    }

-- Monad for type checking
newtype CF a = CF { unCF :: ErrorT String (StateT CFState IO) a }
    deriving (Functor, Monad)

--------------------------------------------------------------------------------
-- CF functions
--------------------------------------------------------------------------------

-- Add a function to the function list
addFunc :: (Type -> CF Type) -> TopDef -> CF ()
addFunc translator (FnDef t id args _) = CF $ do
    t' <- unCF $ translator t
    env <- get
    let funcs = functions env
    case lookup id funcs of
      Nothing -> do args' <- unCF $ mapM getType args
                    put $ env {functions = (id, Fun t' args') : funcs}
      Just _  -> fail $ "Function " ++ printTree id ++ " already declared"
    where getType (Arg t _) = translator t
addFunc _ _ = return ()

-- Lookup a function for its type
lookupFunc :: Ident -> CF Type
lookupFunc id = CF $ do
    env <- get
    case lookup id (functions env) of
      Nothing -> fail $ "Unknown function " ++ printTree id
      Just t  -> return t

-- Check if main exist with correct type signature
checkForMain :: CF Bool
checkForMain = CF $ do
    env <- get
    return (elem (Ident "main", Fun Int []) (functions env))

-- Add a variable to the variable list
addVar :: Ident -> Type -> CF ()
addVar id t = CF $ do
    when (t == Void) $ fail "Illegal void variable declaration"
    env <- get
    let (scope : rest) = locals env
    case lookup id scope of
      Nothing -> put $ env {locals = ((id, t) : scope) : rest}
      Just _  -> fail $ "Variable " ++ printTree id ++ " already declared"

-- Get the variables
getVars :: CF [[(Ident, Type)]]
getVars = CF $ do
    env <- get
    return (locals env)

-- Lookup a variable and get its type
lookupVar :: [[(Ident, Type)]] -> Ident -> Maybe Type
lookupVar vMap id = lookup' vMap
  where lookup' [] = Nothing
        lookup' (scope:rest) =
            case lookup id scope of
              Nothing -> lookup' rest
              Just t  -> Just t

-- Puts the given state function in a new scope
scope :: CF a -> CF a
scope (CF m) = CF $ do
    env <- get
    put $ env {locals = [] : (locals env)}
    a <- m
    env' <- get
    put $ env' {locals = tail (locals env')}
    return a

-- Get return type of current function
getRetType :: CF Type
getRetType = CF $ do
    env <- get
    return (returnType env)

-- Set return type of current function
setRetType :: Type -> CF ()
setRetType t = CF $ do
    modify (\env -> env { returnType = t })

-- Set the return exists state
setReturnExist :: Bool -> CF ()
setReturnExist v = CF $ do
    modify (\env -> env { returnExist = v })

-- Check if a return exists in the given state function
-- or it has already been encountered
existsReturn :: CF a -> CF (a, Bool)
existsReturn (CF m) = CF $ do
    env <- get
    if returnExist env
       then do a <- m
               return (a, True)
       else do a <- m
               env' <- get
               put $ env' { returnExist = False }
               return (a, returnExist env')
