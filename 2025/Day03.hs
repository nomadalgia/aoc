{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day03 where

import Relude

import Data.Char (digitToInt)
import Data.List.Extra (sumOn', elemIndex, dropEnd, maximum)
import Data.Attoparsec.ByteString.Char8 (parseOnly, digit, endOfLine)
import qualified Data.ByteString.Char8 as BS (interact)

main :: IO ()
main = BS.interact (show . solve . parse)

solve = p1 &&& p2

p1, p2 :: [[Int]] -> Int
p1 = sumOn' (digitsToNumber . maxDigitSubseq 2)
p2 = sumOn' (digitsToNumber . maxDigitSubseq 12)

parse :: ByteString -> [[Int]]
parse = fromRight (error "bad input") . parseOnly parser
  where parser = many $ many (digitToInt <$> digit) <* endOfLine

-------------
--- Utils ---
-------------
maxDigitSubseq :: Ord a => Int -> [a] -> [a]
maxDigitSubseq n xs = reverse $ go xs [] (pred n)
  where
    go xs acc 0 = maximum xs : acc
    go xs acc n = let newNumber = maximum $ dropEnd n xs
                      (Just i)  = newNumber `elemIndex` xs
                      xs'       = drop (succ i) xs
                  in go xs' (newNumber:acc) (pred n)

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl' (\acc e -> acc * 10 + e) 0 

-- forwardTuples2 :: [a] -> [(a, a)]
-- forwardTuples2 xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

-- forwardTuples12 :: [a] -> [(a,a,a,a,a,a,a,a,a,a,a,a)]
-- forwardTuples12 xs = do
  -- ( x1: xs1) <- tails xs
  -- ( x2: xs2) <- tails xs1
  -- ( x3: xs3) <- tails xs2
  -- ( x4: xs4) <- tails xs3
  -- ( x5: xs5) <- tails xs4
  -- ( x6: xs6) <- tails xs5
  -- ( x7: xs7) <- tails xs6
  -- ( x8: xs8) <- tails xs7
  -- ( x9: xs9) <- tails xs8
  -- (x10:xs10) <- tails xs9
  -- (x11:xs11) <- tails xs10
  -- x12        <- xs11
  -- return $ (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)

-- maxJoltage :: [Int] -> Int
-- maxJoltage xs = go (reverse xs) 0 0
  -- where
    -- go [] _ best = best
    -- go (x:xs) maxRight best =
      -- let best' = max best (x*10 + maxRight)
          -- maxRight' = max x maxRight
      -- in go xs maxRight' best'

-- combine2 :: (Int, Int) -> Int
-- combine2 = uncurry (+) . first (*10)

-- combine12 :: [Int] -> Int
-- combine12 [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12] =
  -- (x1 * 100000000000) + (x2 * 10000000000) + (x3 * 1000000000) + (x4 * 100000000) + (x5 * 10000000) + (x6 * 1000000) + (x7 * 100000) + (x8 * 10000) + (x9 * 1000) + (x10 * 100) + (x11 * 10) + x12
-- combine12 _ = error "whoops"


-- -- foo xs = go (reverse xs) [] (take 12 $ sortBy (flip compare) xs)
  -- -- where
    -- -- go     [] acc _ = acc
    -- -- go (x:xs) acc sorted | x `elem` sorted = go xs (x:acc) (x `delete` sorted)
                         -- -- | otherwise       = go xs    acc              sorted
