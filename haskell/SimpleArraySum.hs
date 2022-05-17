-- https://www.hackerrank.com/challenges/simple-array-sum/problem

import Text.Printf (printf)

main = do
  _ <- getLine
  ns <- readInts
  let ans = solve ns
  print ans

solve ns = sum ns

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
