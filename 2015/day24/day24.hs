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
  solution <-
    case packSleigh weights of
      Just sol -> return sol
      Nothing -> die "No solution found!"
  printf ("Solution (part 1): "%w%"\n") solution
  printf ("    Quantum entanglement: "%d%"\n")
    (quantumEntanglement (head solution))


quantumEntanglement :: [Int] -> Int
quantumEntanglement = product

packSleigh :: [Int] -> Maybe [[Int]]
packSleigh xs =
  case List.sortBy
       (comparing (length . head) <> comparing (quantumEntanglement . head))
       (splitEvenly 3 xs)
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
  map (\(way, rest) -> (reverse way, reverse rest))
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
