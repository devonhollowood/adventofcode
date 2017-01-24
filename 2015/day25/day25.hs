{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import qualified Data.Maybe as Maybe

main :: IO ()
main = sh $ do
  (r, c) <- options "Day 25" parser
  let code = Maybe.fromJust $ lookup (r, c) codes
  printf ("Code at row "%d%", column "%d%": "%d%"\n") r c code

codes :: [((Int, Int), Integer)]
codes = iterate next_code ((1, 1), 20151125)
  where
    next_code (old_pos, old_n) =
      let next_pos =
            case old_pos of
              (1, c) -> (c + 1, 1)
              (r, c) -> (r - 1, c + 1)
          next_n = old_n * 252533 `rem` 33554393
      in (next_pos, next_n)

parser = (,)
  <$> optInt "row" 'r' "row of code to look up"
  <*> optInt "column" 'c' "column of code to look up"
