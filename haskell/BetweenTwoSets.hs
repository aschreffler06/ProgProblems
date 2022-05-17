-- https://www.hackerrank.com/challenges/between-two-sets/problem

import Text.Printf (printf)
import Control.Monad (replicateM)
import Data.List

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- readInts
  set1 <- readInts
  set2 <- readInts
  print(solve set1 set2)

solve :: [Int] -> [Int] -> Int
solve xs ys = length (nub (intersect (solvex xs max) (solvey ys max)))
    where max = (minimum ys)

solvex :: [Int] -> Int -> [Int]
solvex xs m = [z | z <- [1..m], mult z xs]

mult :: Int -> [Int] -> Bool
mult _ [] = True
mult z (x:xs) | z `mod` x == 0 = mult z xs
              | otherwise = False

solvey :: [Int] -> Int -> [Int]
solvey ys m = [z | z <- [1..m], fac z ys]

fac :: Int -> [Int] -> Bool
fac _ [] = True
fac z (y:ys)  | y `mod` z == 0 = fac z ys
              | otherwise = False
