import Operations
import System.Environment

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

main::IO()
main = do
    line_ <- getLine
    let line  = words line_
    let nb1 = readInt(head line)
    let op = line!!1
    let nb2 = readInt(line!!2)
    case (op, nb1, nb2) of
        ("+", nb1, nb2) -> print (addition nb1 nb2)
        ("-", nb1, nb2) -> print (substraction nb1 nb2)
        ("*", nb1, nb2) -> print (multiply nb1 nb2)
        ("/", nb1, nb2) -> print (divide nb1 nb2)