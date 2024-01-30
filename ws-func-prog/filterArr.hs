--
-- EPITECH PROJECT, 2024
-- ws-func-prog-dajon
-- File description:
-- filterArr
--
import Predicates
where filterArr

filterArr :: (a -> Bool) -> [a] -> [a]
filterArr _ [] = []
filterArr f (a:b)
    | f a = a : filterArr f b
    | otherwise = filterArr f b