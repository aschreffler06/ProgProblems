-- https://www.hackerrank.com/challenges/staircase/problem

import Text.Printf (printf)

readInts :: IO [Integer]
readInts = fmap (fmap read . words) getLine

main = do
  n <- readLn :: IO Int
  mapM_ putStrLn (staircase n n)

staircase :: Int -> Int -> [String]
staircase _ 0 = []
staircase original n = do
  let i = (replicate (n - 1) ' ') ++ (replicate (original - n + 1) '#')
  i:(staircase original (n - 1))
