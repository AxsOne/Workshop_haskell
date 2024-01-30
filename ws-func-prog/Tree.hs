module Tree where
import Distribution.Simple.Build (build)
data ASTree = Add ASTree ASTree
            | Sub ASTree ASTree
            | Mul ASTree ASTree
            | Div ASTree ASTree
            | Value Int

instance Show ASTree where
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Value n)   = show n

buildExpr :: ASTree -> String -> ASTree -> ASTree
buildExpr e1 op e2
    | op == "+" = Add e1 e2
    | op == "-" = Sub e1 e2
    | op == "*" = Mul e1 e2
    | op == "/" = Div e1 e2
    | otherwise = error "Invalid operator"

isNum :: Char -> Bool
isNum c
    | c `elem` ['1','2','3','4','5','6','7','8','9','0', '-'] = True
    | otherwise = False

myVerifInt :: [Char] -> Bool
myVerifInt [] = True
myVerifInt (a:b)
    | isNum a = myVerifInt b
    | otherwise = False

readInt :: String -> Int
readInt [] = 0
readInt list
    | myVerifInt list = read list :: Int
    | otherwise = 0

mapValues :: [String] -> [ASTree]
mapValues = map (Value . readInt)


buildExprList :: [String] -> [ASTree] -> ASTree
buildExprList [a] [b, c] = buildExpr b a c
buildExprList (a:b) (c:d) = buildExprList b (buildExpr c a (head d) : tail d)

buildAST :: ([String], [String]) -> ASTree
buildAST (a, b) = buildExprList a (mapValues b)

treeTraversal :: ASTree -> Int
treeTraversal (Add e1 e2) = treeTraversal e1 + treeTraversal e2
treeTraversal (Sub e1 e2) = treeTraversal e1 - treeTraversal e2
treeTraversal (Mul e1 e2) = treeTraversal e1 * treeTraversal e2
treeTraversal (Div e1 e2) = treeTraversal e1 `div` treeTraversal e2
treeTraversal (Value n) = n