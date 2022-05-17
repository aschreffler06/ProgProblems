-- https://www.hackerrank.com/challenges/strange-advertising/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  days <- readLn :: IO Int
  print(solve days 5)



solve :: Int -> Int -> Int
solve 0 _ = 0
solve numDays p = likes + (solve (numDays - 1) (likes * 3))
  where likes = p `div` 2
