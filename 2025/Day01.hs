{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Day01 where

import Relude
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

main :: IO ()
main = T.interact (show . solve . parse)

solve :: [Int] -> (Int, Int)
solve = p1 &&& p2

p1, p2, common :: [Int] -> Int
p1 = common
p2 = common . concatMap unitList
common = length . filter (==0) . scanl (wrapAdd 100) 50

parse :: Text -> [Int]
parse = map fst
      . rights
      . map (T.signed T.decimal)
      . T.lines
      . T.map (\case 'R' -> '+';
                     'L' -> '-';
                      c   -> c )

-------------
--- Utils ---
-------------
wrapAdd :: Int -> Int -> Int -> Int
wrapAdd m a b = (a + b) `mod` m

unitList :: Int -> [Int]
unitList n = replicate (abs n) (signum n)
