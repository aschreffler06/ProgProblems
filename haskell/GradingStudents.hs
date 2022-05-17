-- https://www.hackerrank.com/challenges/grading/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

main = do
  n <- readLn :: IO Int
  ns <- replicateM n (readLn :: IO Int)
  let ans = (map grade ns)
  putStr $ unlines (fmap show ans)

grade :: Int -> Int
grade x | x < 38 = x
        | 5 - y < 3 = (x + (5 - y))
        | 5 - y >= 3 = x
  where y = (x `mod` 5)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
