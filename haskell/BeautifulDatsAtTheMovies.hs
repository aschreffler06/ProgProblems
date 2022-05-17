-- https://www.hackerrank.com/challenges/beautiful-days-at-the-movies/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  x <- readInts
  let k = x !! 2
  let i = x !! 0
  let j = x !! 1
  print(solve [i..j] k)

solve :: [Int] -> Int -> Int
solve xs k = length [x | x <- xs, (x - reverseInt x) `mod` k == 0]



reverseInt :: Int -> Int
reverseInt n = read (reverse (show n)) :: Int
