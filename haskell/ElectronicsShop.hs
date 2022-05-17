-- https://www.hackerrank.com/challenges/electronics-shop/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  x <- readInts
  let budget = x !! 0
  kb <- readInts
  usb <- readInts
  print(solve budget kb usb)

solve :: Int -> [Int] -> [Int] -> Int
solve b ks us = do
  let arr = (map addT [ (k, u) | k <- ks, u <- us, (k + u) <= b])
  if length arr == 0 then -1 else maximum arr

addT :: (Int, Int) -> Int
addT (i, j) = i + j
