-- https://www.hackerrank.com/challenges/migratory-birds/problem

import Text.Printf (printf)
import Control.Monad (replicateM)
import Data.List

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  birds <- readInts
  print(solve birds)

solve :: [Int] -> Int
solve xs = do
  let sorted = sort (splitArr xs)
  let maxLen = maximum (map length (sorted))
  minimum (concat ([xs | xs <- sorted, (length xs) == maxLen]))

splitArr :: [Int] -> [[Int]]
splitArr [] = [[]]
splitArr xs = (filter (== (xs !! 0)) xs):(splitArr (filter (/= (xs !! 0)) xs))
