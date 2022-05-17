-- https://www.hackerrank.com/challenges/birthday-cake-candles/problem

import Text.Printf (printf)
import Data.List

main = do
  _ <- getLine
  xs <- readInts
  let list = reverse (sort xs)
  let n = head list
  print (solve list n)

solve xs n = sum [1 | x <- xs, x == n]
  


readInts :: IO [Integer]
readInts = fmap (fmap read . words) getLine
