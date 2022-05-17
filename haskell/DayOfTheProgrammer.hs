-- https://www.hackerrank.com/challenges/day-of-the-programmer/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  year <- readLn :: IO Int
  putStrLn(solve year)

solve :: Int -> String
solve year | year == 1918 = "26.09.1918"
           | year `mod` 4 == 0 && year < 1917 = "12.09." ++ show(year)
           | year `mod` 4 == 0 && year `mod` 100 /= 0 = "12.09." ++ show(year)
           | year `mod` 400 == 0 = "12.09." ++ show(year)
           | otherwise = "13.09." ++ show(year)
