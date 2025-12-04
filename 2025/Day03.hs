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
