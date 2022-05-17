-- https://www.hackerrank.com/challenges/diagonal-difference/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

main = do
  n <- readLn :: IO Int
  nss <- replicateM n readInts
  print(solve nss)

solve :: [[Int]] -> Int
solve nss = abs(leftDiag nss - rightDiag nss)

leftDiag :: [[Int]] -> Int
leftDiag [] = 0
leftDiag (xs:xss) = (head xs) + leftDiag ((map tail) xss)

rightDiag :: [[Int]] -> Int
rightDiag xss = leftDiag (map reverse xss)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine
