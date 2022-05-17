-- https://www.hackerrank.com/challenges/cut-the-sticks/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  _ <- getLine
  sticks <- readInts
  mapM_ print (solve sticks)

solve :: [Int] -> [Int]
solve [] = []
solve xs = (length xs):(solve (cut xs))

cut :: [Int] -> [Int]
cut xs = map (subtract m) (filter (/= m) xs)
  where m = minimum xs
