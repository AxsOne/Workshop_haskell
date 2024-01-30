import Operations
import System.Environment
import Operations
import filterArr
import parseLine
import Predicates
import Tree

verifInt :: [Char] -> Bool
verifInt [] = True
verifInt (x:xs)
    | (x >= '0' || x <= '9') = verifInt xs
    | otherwise = False

readInt :: String -> Int
readInt [] = 0
readInt  list
    |   verifInt list = read list :: Int
    |   otherwise = 0

main :: IO ()
main = do
    value <- getLine
    value_ :: [String] <- return (words value)
    print (treeTraversal(buildAST (parseLine value_)))