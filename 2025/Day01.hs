{-# LANGUAGE NoImplicitPrelude #-}
module Day01 where

import Relude
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T
import qualified Data.Map       as M

main :: IO ()
main = T.interact (show . solve . parse)

solve :: [Int] -> (Int, Int)
solve = p1 &&& p2

p1, p2, common :: [Int] -> Int
p1 = common
p2 = common . concatMap unitList
common = length . filter (0==) . scanl (wrapAdd 100) 50

parse :: Text -> [Int]
parse = map fst . rights . map (T.signed T.decimal) . T.lines . replaceMany [('R', '+'), ('L', '-')]

-------------
--- Utils ---
-------------
wrapAdd :: Int -> Int -> Int -> Int
wrapAdd modulus a b = (a + b) `mod` modulus

replaceMany :: [(Char, Char)] -> Text -> Text
replaceMany rules = T.map (\c -> M.findWithDefault c c (M.fromList rules))

unitList :: Int -> [Int]
unitList n | n == 0    = []
           | otherwise = replicate (abs n) (signum n)