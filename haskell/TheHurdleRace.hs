-- https://www.hackerrank.com/challenges/the-hurdle-race/problem

import Text.Printf (printf)
import Data.List

main = do
  [x, n] <- readInts
  ns <- readInts
  print(solve n ns)

solve n ns = if x >= 0 then x else 0
  where x = head(reverse(sort ns)) - n


readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
