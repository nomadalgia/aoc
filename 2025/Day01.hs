{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Day01 where

import Relude
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

main :: IO ()
main = T.interact (show . solve . parse)

solve :: [Integer] -> (Integer, Integer)
solve = (p1 &&& p2) . common

p1, p2 :: [(Integer, Integer)] -> Integer
p1 = genericLength . filter (==0) . map snd
p2 = maybe 0 fst . viaNonEmpty last

common :: [Integer] -> [(Integer, Integer)]
common = scanl (\(n,a) b -> first ((n+) . abs) $ (a + b) `divMod` 100) (0,50)

parse :: Text -> [Integer]
parse = rights
      . map (fmap fst . T.signed T.decimal)
      . T.lines
      . T.map (\case 'R' -> '+';
                     'L' -> '-';
                      c   -> c )
