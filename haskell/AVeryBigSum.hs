-- https://www.hackerrank.com/challenges/a-very-big-sum/problem

import Text.Printf (printf)

main = do
  _ <- getLine
  ns <- readInts
  let ans = sum ns
  print(ans)


readInts :: IO [Integer]
readInts = fmap (fmap read . words) getLine
