{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day02 where

import Relude

import Data.Char
import Data.Attoparsec.ByteString.Char8 (parseOnly, char, sepBy, decimal)
import Data.List.Extra                  (chunksOf, allSame)
import qualified Data.ByteString as BS  (interact)

main :: IO ()
main = BS.interact (show . solve . parse)

solve :: [(Integer, Integer)] -> (Integer, Integer)
solve = (p1 &&& p2) . common

p1, p2 :: [[Integer]] -> Integer
p1 = sum . concatMap (filter (isDoubled . show))
p2 = sum . concatMap (filter (isRepeated . show))

common :: [(Integer, Integer)] -> [[Integer]]
common = map $ uncurry enumFromTo

parse :: ByteString -> [(Integer, Integer)]
parse = fromRight (error "impossible") . parseOnly parser
  where parser  = ((,) <$> decimal <* char '-' <*> decimal) `sepBy` char ','

-------------
--- Utils ---
-------------
isDoubled :: Eq a => [a] -> Bool
isDoubled xs = uncurry (==) $ (length xs `div` 2) `splitAt` xs

isRepeated :: Eq a => [a] -> Bool
isRepeated xs = any allSame $ map (`chunksOf` xs) [1 .. length xs `div` 2]
