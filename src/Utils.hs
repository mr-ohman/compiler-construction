
module Utils where

-- Extra utilites for the Javalette compiler

import AbsJavalette
import PrintJavalette


-- Gets the type from an annotated expression
getType :: Expr -> Type
getType (Ann t _) = t

-- Zip two lists in an monad an fail with a message if the two list are of
-- different lengths
strictZip :: Monad m => [a] -> [b] -> String -> m [(a, b)]
strictZip (a:as) (b:bs) msg = do cs <- strictZip as bs msg
                                 return $ (a, b) : cs
strictZip [] [] _ = return []
strictZip _ _ msg = fail msg

-- Concat two strings with a whitespece between them
(+-+) :: String -> String -> String
(+-+) a b = a ++ " " ++ b

-- Pretty print a list separated by "," and "or"
printOr :: Print a => [a] -> String
printOr [] = error "printOr: Empty type list not allowed"
printOr (t:[]) = printTree t
printOr (t1:(t2:[])) = printTree t1 ++ " or " ++ printTree t2
printOr (t1:(t2:ts)) = printTree t1 ++ ", " ++ printOr (t2:ts)

-- Get the content of an ArrayIndex
innerExpr :: ArrayIndex -> Expr
innerExpr (AIndex a) = a

-- Make an array type with inner type t and dimension n
arrayLevel :: Type -> Int -> Type
arrayLevel t n = foldr (.) id (replicate n Array) t

-- Remove n dimension from an array
removeLevel :: Int -> Type -> Maybe Type
removeLevel 0 t = Just t
removeLevel n (Array t) = removeLevel (n-1) t
removeLevel _ _ = Nothing

-- Seperate a list with a seperator
seperate :: [a] -> [[a]] -> [a]
seperate s (x:(y:xs)) = x ++ s ++ seperate s (y:xs)
seperate s (x:[]) = x
seperate s [] = []
