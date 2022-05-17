-- https://www.hackerrank.com/challenges/plus-minus/problem

import Text.Printf (printf)

main = do
  _ <- getLine
  ns <- readInts
  print(solvePos ns)
  print(solveNeg ns)
  print(solveZero ns)

solvePos xs = fdiv (length([1 | x <- xs, x > 0])) (length xs)
solveNeg xs = fdiv (length([1 | x <- xs, x < 0])) (length xs)
solveZero xs = fdiv (length([1 | x <- xs, x == 0])) (length xs)

fdiv a b = (fromIntegral a) / (fromIntegral b)


readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
