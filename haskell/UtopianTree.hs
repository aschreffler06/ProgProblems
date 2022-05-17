-- https://www.hackerrank.com/challenges/utopian-tree/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  n <- readLn :: IO Int
  testCases <- replicateM n (readLn :: IO Int)
  mapM_ print (solve testCases 1)

solve :: [Int] -> Int -> [Int]
solve [] _ = []
solve (x:xs) height = (growth x height):(solve xs height)

growth :: Int -> Int -> Int
growth numCases height = altN numCases double add1 height

altN :: Int -> (Int -> Int) -> (Int -> Int) -> Int -> Int
altN 0 _ _ x = x
altN n f g x = altN (n - 1) g f (f x)

double :: Int -> Int
double n = n * 2

add1 :: Int -> Int
add1 n = n + 1
