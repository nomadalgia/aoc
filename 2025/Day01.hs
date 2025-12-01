{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Day01 where

import Relude
import Data.Composition
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

main :: IO ()
main = T.interact (show . solve . parse)

solve :: [Int] -> (Int, Int)
solve = p1 &&& p2

p1, p2 :: [Int] -> Int
p1 = length . filter (==0) . scanl (flip mod 100 .: (+)) 50
p2 = fst . foldl' (\(n,a) b -> first ((n+) . abs) $ (a + b) `divMod` 100) (0,50)

parse :: Text -> [Int]
parse = map fst
      . rights
      . map (T.signed T.decimal)
      . T.lines
      . T.map (\case 'R' -> '+';
                     'L' -> '-';
                      c   -> c )
