-- https://www.hackerrank.com/challenges/cats-and-a-mouse/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

main = do
  n <- readLn :: IO Int
  nss <- replicateM n readInts
  let ans = map solve nss
  putStr (unlines ans)

solve :: [Int] -> String
solve pos | abs(a - c) < abs(b - c) = "Cat A"
          | abs(a - c) > abs(b - c) = "Cat B"
          | otherwise = "Mouse C"
          where a = head pos
                b = head (tail pos)
                c = head (reverse pos)


readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
