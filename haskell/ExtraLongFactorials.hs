-- https://www.hackerrank.com/challenges/extra-long-factorials/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  n <- readLn :: IO Integer
  print(fac n)

fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)
