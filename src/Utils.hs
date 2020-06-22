module Utils where

space :: Int -> String
space n = take n (repeat ' ')

head' :: [a] -> [a]
head' (x:xs) = [x]
head' x      = x
