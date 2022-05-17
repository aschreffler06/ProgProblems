-- https://www.hackerrank.com/challenges/sock-merchant/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  socks <- readInts
  print (solve socks)

solve :: [Int] -> Int
solve [] = 0
solve (x:xs) = pairs + (solve removed)
  where removed = filter (/= x) (x:xs)
        pairs = (length (filter (== x) (x:xs))) `div` 2
