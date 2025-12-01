{-# LANGUAGE NoImplicitPrelude #-}
module Day01 where

import Relude
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T
import qualified Data.Map       as M

main :: IO ()
main = T.interact (show . solve . parse)

solve = p1 &&& p2

p1 :: [Int] -> Int
p1 = length . filter (0==) . scanl wrapDial 50
  where wrapDial a b = (a + b) `mod` 100
p2 = id

parse :: Text -> [Int]
parse = map fst . rights . map (T.signed T.decimal) . T.lines . replaceMany [('R', '+'), ('L', '-')]

-------------
--- Utils ---
-------------
replaceMany :: [(Char, Char)] -> Text -> Text
replaceMany rules = T.map (\c -> M.findWithDefault c c (M.fromList rules))