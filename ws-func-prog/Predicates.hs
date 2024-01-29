--
-- EPITECH PROJECT, 2024
-- ws-func-prog-dajon
-- File description:
-- Predicates
--
module Predicates where

isDigit :: [Char] -> Bool
isDigit [] = True
isDigit (x:xs)
    | (x >= '0' || x <= '9') = isDigit xs
    | otherwise = False

isString :: [Char] -> Bool
isString [] = True
isString (x:xs) =
    case x of
        '+' -> isString xs
        '-' -> isString xs
        '*' -> isString xs
        '/' -> isString xs
        _ -> False

