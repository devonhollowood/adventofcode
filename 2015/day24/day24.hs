{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import qualified Data.List as List
import Data.Ord (comparing)

main :: IO ()
main = sh $ do
  filename <- options "Day 24" parser
  contents <- liftIO $ readTextFile filename
  weights <-
    case match (decimal `sepBy` newline <* newline <* eof) contents of
      [x] -> return x
      [] -> die "failed to parse weights"
      matches -> die $ format ("ambiguous weights parse: "%w) matches
  three_way_solution <-
    case packSleigh 3 weights of
      Just sol -> return sol
      Nothing -> die "No part 1 solution found!"
  printf ("Solution (part 1): "%w%"\n") three_way_solution
  printf ("    Quantum entanglement: "%d%"\n")
    (quantumEntanglement (head three_way_solution))
  four_way_solution <-
    case packSleigh 4 weights of
      Just sol -> return sol
      Nothing -> die "No part 2 solution found!"
  printf ("Solution (part 2): "%w%"\n") four_way_solution
  printf ("    Quantum entanglement: "%d%"\n")
    (quantumEntanglement (head four_way_solution))


quantumEntanglement :: [Int] -> Int
quantumEntanglement = product

packSleigh :: Int -> [Int] -> Maybe [[Int]]
packSleigh n xs =
  case splitEvenly n xs
  of
    (way : _) -> Just way
    [] -> Nothing

splitEvenly ::
  Int -- number of ways to split
  -> [Int] -- ascending numbers to split
  -> [[[Int]]] -- list of ways to split
splitEvenly 0 [] = [[]]
splitEvenly 0 _ = []
splitEvenly _ [] = []
splitEvenly n xs =
  let target = sum xs `div` n
  in concatMap
     (\(filled, rem) -> map (filled :) $ splitEvenly (n - 1) rem)
     (fillToN target xs)

fillToN ::
  Int -- amount to fill to
  -> [Int] -- ascending list to fill from
  -> [([Int], [Int])] -- [(way to fill to `target`, remaining list)]
fillToN target =
  List.sortBy
  (comparing (length . fst) <> comparing (quantumEntanglement . fst))
  . map (\(way, rest) -> (reverse way, reverse rest))
  . go 0 [] []
  where
    go sum chosen unchosen (x : xs)
      | x > (target - sum) = []
      | x == (target - sum) = [(x : chosen, foldl (flip (:)) unchosen xs)]
      | otherwise =
        go (sum + x) (x : chosen) unchosen xs
        ++ go sum chosen (x : unchosen) xs
    go _ _ _ [] = []

parser = argPath "numbers" "file containing numbers"
