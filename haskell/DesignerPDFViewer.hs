-- https://www.hackerrank.com/challenges/designer-pdf-viewer/problem

import Text.Printf (printf)
import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  letterMap <- readInts
  letters <- getLine
  print(solve letters letterMap)

solve :: String -> [Int] -> Int
solve letters letterMap = do 
  let ans = map snd [x | x <- zip ['a'..'z'] letterMap, l <- letters, l == (fst x)]
  (*) (maximum ans) (length ans)
