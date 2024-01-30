--
-- EPITECH PROJECT, 2024
-- ws-func-prog-dajon
-- File description:
-- Predicates
--
module Predicates where
import Data.Char (isDigit)


predicate :: String -> Bool
predicate [] = True
predicate (a:b)
    | isDigit a = predicate b
    | otherwise = False