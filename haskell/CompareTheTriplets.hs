-- https://www.hackerrank.com/challenges/compare-the-triplets/problem

import Text.Printf (printf)

main = do
  [a, b, c] <- readInts
  [x, y, z] <- readInts
  let (alice, bob) = solve a b c x y z
  printf "%d %d\n" alice bob

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

solve :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
solve a b c x y z = (compare a x + compare b y + compare c z,
                     compare x a + compare y b + compare z c)
      where compare m n = if (m > n) then 1 else 0
