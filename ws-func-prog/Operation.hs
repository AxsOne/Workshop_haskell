module Operations where

addition :: Int -> Int -> Int
addition = (+)


substraction :: Int -> Int -> Int
substraction = (-)

multiply :: Int -> Int -> Int
multiply = (*)

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x s = Just (div x s)
