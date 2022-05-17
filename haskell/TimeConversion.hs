-- https://www.hackerrank.com/challenges/time-conversion/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  time <- getLine
  let newTime = solve time
  if length newTime == 7 then putStrLn("0" ++ newTime) else putStrLn newTime

solve :: String -> String
solve time = do
  let hour = read (take 2 time) :: Int
  let m = lastN 2 time
  let strippedTime = drop 2 (init(init time))
  if m == "PM" then if hour == 12 then show hour ++ strippedTime else show (hour + 12) ++ strippedTime
  else if hour == 12 then "00" ++ strippedTime else show hour ++ strippedTime

lastN :: Int -> String -> String
lastN n (x:xs) = if n == length (x:xs) then (x:xs) else lastN n xs
