where parseLine
import Predicates
parseLine :: [String] -> ([String], [String])
parseLine [] = ([],[])
parseLine [a]
    | predicate a = ([], [a])
    | otherwise = ([a], [])
parseLine (a:b)
    | predicate a = (fst (parseLine b), a : snd (parseLine b))
    | otherwise = (a : fst (parseLine b), snd (parseLine b))