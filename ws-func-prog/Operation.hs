module Operations where

addition :: Int -> Int -> Int
addition a b = a + b

-- substraction
substraction :: Int -> Int -> Int
substraction a b = a - b

-- multiply
multiply :: Int -> Int -> Int
multiply a b = a * b

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x s = Just (x `div` s)