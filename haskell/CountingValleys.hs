-- https://www.hackerrank.com/challenges/counting-valleys/problem?h_r=profile

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  steps <- getLine
  print(solve steps 0)

solve :: String -> Int -> Int
solve [] _ = 0
solve steps lev | (head steps) == 'U' && lev == -1 = 1 + solve (tail steps) (lev + 1)
                | (head steps) == 'U' = solve (tail steps) (lev + 1)
                | (head steps) == 'D' = solve (tail steps) (lev - 1)
                | otherwise = 0
