module Search (
  dfs,
  bfs
  ) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.List as List

bfs :: Ord state =>
  (state -> [state]) -- generate next states
  -> (state -> Bool) -- determine if answer found
  -> [(state -> Bool)] -- functions which prune on True
  -> state -- initial state
  -> Maybe [state]
bfs = search Seq.empty

dfs :: Ord state =>
  (state -> [state]) -- generate next states
  -> (state -> Bool) -- determine if answer found
  -> [(state -> Bool)] -- functions which prune on True
  -> state -- initial state
  -> Maybe [state]
dfs = search []

search :: (Ord state, SearchContainer f) =>
  f state -- empty SearchContainer
  -> (state -> [state]) -- generate next states
  -> (state -> Bool) -- determine if answer found
  -> [(state -> Bool)] -- functions which prune on True
  -> state -- initial state
  -> Maybe [state]
search empty next pred prunes init = reverse <$> go (Map.singleton init []) empty init
  where
    go visited queue current
      | pred current = Map.lookup current visited
      | otherwise =
        let steps_so_far = Maybe.fromJust $ Map.lookup current visited
            new_states =
              Map.fromList . map (\st -> (st, st:steps_so_far)) $ next current
            new_visited = Map.unionWith shorter visited new_states
            new_queue =
              List.foldl' push queue .
               (filter (\st -> (not $ st `Map.member` visited || any ($ st) prunes)))
               $ Map.keys new_states
        in pop new_queue >>= (\(x, xs) -> go new_visited xs x)
    shorter xs ys
      | length xs <= length ys = xs
      | otherwise = ys

class SearchContainer f where
  pop :: f a -> Maybe (a, f a)
  push :: f a -> a -> f a

instance SearchContainer Seq.Seq where
  pop seq =
    case Seq.viewl seq of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
  push seq a = a Seq.<| seq

instance SearchContainer [] where
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list
