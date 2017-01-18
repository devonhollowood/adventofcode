{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.MemoCombinators as Memo
import Turtle
import qualified Data.Maybe as Maybe
import qualified Data.List as List

main :: IO ()
main = sh $ do
  input <- options "Day 20" parser
  printf ("First house with greater than "%d%" presents (part 1): "%d%"\n")
    input
    (Maybe.fromJust $ List.find ((>= input) . (* 11) . sum . factors) [1 .. ])
  printf ("First house with greater than "%d%" presents (part 2): "%d%"\n")
    input
    (Maybe.fromJust $ List.find
      ((>= input) . (* 11) . sum . factorsWithinMultiple 50)
      [1 .. ]
    )

factors :: Int -> [Int]
factors = Memo.integral (go 2)
  where
    go test target
      | target == 1 = [1]
      | test * test == target = [1, test, target]
      | test * test > target = [1, target]
      | target `rem` test == 0 =
        let facts = factors (target `div` test)
        in merge (facts) (map (* test) facts)
      | otherwise = go (test + 1) target
    merge (x:xs) (y:ys)
      | x < y = x : merge xs (y : ys)
      | x == y = x : merge xs ys
      | otherwise = y : merge (x : xs) ys
    merge xs [] = xs
    merge [] ys = ys

factorsWithinMultiple :: Int -> Int -> [Int]
factorsWithinMultiple mult n =
  map (\m -> n `div` m) $ (filter (\m -> n `rem` m == 0)) [mult, mult - 1 .. 1]

parser = argInt "input" "Number of presents for which to solve"
