-- https://www.hackerrank.com/challenges/apple-and-orange/problem

import Text.Printf (printf)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  [start, end] <- readInts
  trees <- readInts
  let appleTree = trees !! 0
  let orangeTree = trees !! 1
  nums <- readInts
  let numApples = nums !! 0
  let numOranges = nums !! 1
  apples <- readInts
  oranges <- readInts
  let applesPos = getPos appleTree apples
  let orangesPos = getPos orangeTree oranges
  print(solve start end applesPos)
  print(solve start end orangesPos)
  



getPos :: Int -> [Int] -> [Int]
getPos x xs = map (+ x) (xs)

solve :: Int -> Int -> [Int] -> Int
solve s e xs = length [x | x <- xs, x >= s, x <= e]
