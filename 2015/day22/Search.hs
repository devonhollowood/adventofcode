module Search (
  dfs,
  bfs,
  djikstra
  ) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List

djikstra :: (Ord state, Num cost, Ord cost) =>
  (state -> [(cost, state)]) -- generate next states with incremental cost
  -> (state -> Bool) -- determine if answer found
  -> [(state -> Bool)] -- functions which prune states on True
  -> state -- initial state
  -> Maybe (cost, [(cost, state)]) -- total cost and steps
djikstra next pred prunes init =
  go (Map.singleton init (0, [])) Set.empty init
  where
    go visited queue current
      | pred current =
        (\(cost, steps) -> (cost, reverse steps))
        <$> Map.lookup current visited
      | otherwise =
        let (old_cost, old_steps) = Maybe.fromJust $ Map.lookup current visited
            new_cost_steps =
              Map.fromList
              . map (\(incr, st) ->
                       let new_cost = old_cost + incr
                       in (st, (new_cost, (incr, st) : old_steps))
                    )
              $ next current
            new_visited = Map.unionWith less_costly visited new_cost_steps
            new_queue =
              List.foldl' push queue
              . map (\(st, (cost, _)) -> (cost, st))
              . (filter (\(st, _) ->
                         (not $ st `Map.member` visited
                          || any ($ st) prunes
                         )
                      ))
               $ Map.toList new_cost_steps
        in pop new_queue >>= (\((_, st), queue') -> go new_visited queue' st)
    less_costly a b = if fst a <= fst b then a else b

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
  pop :: Ord a => f a -> Maybe (a, f a)
  push :: Ord a => f a -> a -> f a

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

instance SearchContainer Set.Set where
  pop = Set.minView
  push = flip Set.insert
