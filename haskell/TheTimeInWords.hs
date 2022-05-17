-- https://www.hackerrank.com/challenges/the-time-in-words/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  hr <- readLn :: IO Int
  minute <- readLn :: IO Int
  putStrLn (solve hr minute)

solve :: Int -> Int -> String
solve h m | m == 0 = (show' h) ++ " o' clock"
          | m == 15 = "quarter past " ++ (show' h)
          | m == 30 = "half past " ++ (show' h)
          | m == 45 = "quarter to " ++ (show' (h + 1))
          | m == 1 = (show' m) ++ " minute past " ++ (show' h)
          | m == 59 = (show' 1) ++ " minute to " ++ (show' (h + 1))
          | m < 30 = (show' m) ++ " minutes past " ++ (show' h)
          | m > 30 = (show' (60 - m)) ++ " minutes to " ++ (show' (h + 1))
          | otherwise = ""

show' :: Int -> String
show' 1 = "one"
show' 2 = "two"
show' 3 = "three"
show' 4 = "four"
show' 5 = "five"
show' 6 = "six"
show' 7 = "seven"
show' 8 = "eight"
show' 9 = "nine"
show' 10 = "ten"
show' 11 = "eleven"
show' 12 = "twelve"
show' 13 = "thirteen"
show' 14 = "fourteen"
show' 16 = "sixteen"
show' 17 = "seventeen"
show' 18 = "eighteen"
show' 19 = "ninteen"
show' 20 = "twenty"
show' 21 = "twenty one"
show' 22 = "twenty two"
show' 23 = "twenty three"
show' 24 = "twenty four"
show' 25 = "twenty five"
show' 26 = "twenty six"
show' 27 = "twenty seven"
show' 28 = "twenty eight"
show' 29 = "twenty nine"
