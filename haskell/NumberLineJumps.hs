-- https://www.hackerrank.com/challenges/kangaroo/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  input <- readInts
  let k1s = input !! 0
  let k1j = input !! 1
  let k2s = input !! 2
  let k2j = input !! 3 
  putStrLn(solve k1s k1j k2s k2j)

solve :: Int -> Int -> Int -> Int -> String
solve k1p k1j k2p k2j | k2p < k1p = "NO"
                      | k2j >= k1j = "NO"
                      | k1p == k2p = "YES"
                      | otherwise = solve (k1p + k1j) k1j (k2p + k2j) k2j
