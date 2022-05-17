-- https://www.hackerrank.com/challenges/find-digits/problem

import Text.Printf (printf)
import Control.Monad (replicateM)
import Data.Char

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  n <- readLn :: IO Int
  cases <- replicateM n (readLn :: IO Int)
  mapM_ print (solve n cases)

solve :: Int -> [Int] -> [Int]
solve 0 _ = []
solve n (x:xs) = (countDigits x):(solve (n - 1) xs)

countDigits :: Int -> Int
countDigits n = length (filter (== True) (facDigits n (show n)))

facDigits :: Int -> [Char] -> [Bool]
facDigits _ [] = []
facDigits n (x:xs) | x == '0' = facDigits n xs
                   | otherwise = (n `mod` (digitToInt x) == 0):(facDigits n xs)
