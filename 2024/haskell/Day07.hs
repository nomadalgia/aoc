module Day07 where

import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import Data.Composition
import Data.Bifunctor
import Data.Function

type Op = Int -> Int -> Int

main :: IO ()
main = interact (show . solve . parse)

parse :: String -> [(Int, [Int])]
parse = mapMaybe parseLine . lines
  where parseLine = uncons . map read . words . delete ':'

solve :: [(Int, [Int])] -> (Int, Int)
solve = p1 &&& p2

p1, p2 :: [(Int, [Int])] -> Int
p1 = common [(+), (*)]
p2 = common [(+), (*), flip comb]

common :: [Op] -> [(Int, [Int])] -> Int
common ops = sumOn' fst . filter (\(r, xs) -> r `elem` gen xs ops)

-------------
--- Utils ---
-------------
gen :: [Int] -> [Op] -> [Int]
gen xs ops = go $ reverse xs where
  go []     = []
  go [x]    = [x]
  go (x:xs) = [op x y | y <- go xs, op <- ops]

comb :: Int -> Int -> Int
comb = read .: (++) `on` show
