-- https://www.hackerrank.com/challenges/bon-appetit/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  x <- readInts
  let n = x !! 1
  items <- readInts
  annaPaid <- readLn :: IO Int
  let ans = solve n items annaPaid
  if ans == 0 then putStrLn("Bon Appetit") else print(ans)

removeNth :: Int -> [Int] -> [Int]
removeNth _ [] = []
removeNth n (x:xs) | n == 0 = xs
                   | otherwise = x:(removeNth (n-1) xs)

solve :: Int -> [Int] -> Int -> Int
solve n items anna = do
  let new = removeNth n items
  anna - ((sum new) `div` 2)
