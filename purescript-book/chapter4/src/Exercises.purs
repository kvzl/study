module Exercises where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, head, length, tail, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)

-- Exercise 4.4.1
isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

-- Exercise 4.4.2
countEven :: Array Int -> Int
countEven arr =
  case x of
    Just n ->
      if isEven n
        then 1 + countEven xs
        else countEven xs
    Nothing -> 0
  where
    x = head arr
    xs = fromMaybe [] $ tail arr


-- Exercise 4.7.1
toSquare :: Array Int -> Array Int
toSquare = map (\n -> n * n)

-- Exercise 4.7.2
removeNegative :: Array Int -> Array Int
removeNegative = filter (\n -> n > -1)

-- Exercise 4.7.3
infixr 8 filter as <$?>

removeNegative' :: Array Int -> Array Int
removeNegative' arr = (\n -> n > -1) <$?> arr

-- Exercise 4.11.1
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime n = (length $ factors n) < 2

-- Exercise 4.11.2
product :: Array Int -> Array Int -> Array (Array Int)
product xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

product' :: Array Int -> Array Int -> Array (Array Int)
product' xs ys =
  concatMap (\x ->
    map (\y -> [x, y]) ys
  ) xs

-- Exercise 4.11.3
triples :: Int -> Array (Array Int)
triples n | n < 1 = []
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- 0 .. n
  guard $ (a * a + b * b == c * c)
  pure [a, b, c]

-- Exercise 4.11.4
factorizations :: Int -> Array (Array Int)
factorizations n
  | isPrime n = [[n]]
  | otherwise = [n] : do
    x <- helper n
    xs <- factorizations $ n / x
    pure $ x : xs

    where
      -- generate factors excluding 1 and n
      helper m = do
        x <- 1 .. m
        guard $ (n `mod` x) == 0 && x > 1 && x < m
        pure x

-- Exercise 4.15.1
allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc n -> acc && n == true) true

-- Exercise 4.15.3
fib'' :: Int -> Int
fib'' n = fib' n 0 1
  where
    fib' 0 a b = 0
    fib' 1 a b = b
    fib' m a b = fib' (m - 1) b (a + b)

-- Exercise 4.15.4
reverseL :: forall a. Array a -> Array a
reverseL = foldl (\xs x -> x : xs) []

