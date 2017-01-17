{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import qualified Text.Megaparsec as Mpc
import qualified Text.Megaparsec.Text as Mpc
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

main = sh $ do
  fname <- options "Day 19" parser
  contents <- liftIO $ readTextFile fname
  (subs, str) <-
        case Mpc.parse (parseInput <* Mpc.eof)
             (Text.unpack $ format fp fname) contents of
          Left parse_err -> die $ Text.pack (Mpc.parseErrorPretty parse_err)
          Right grid -> return grid
  printf ("Number of subs (Part 1): "%d%"\n") (Set.size $ performSubs subs str)
  printf ("Number of steps from `e` to `"%s%"` (Part 2): "%s%"\n") str $
    case bfs
         (Set.toList . performSubs subs)
         (== str)
         [\try -> Text.length try > Text.length str]
         "e"
    of
      Just steps -> format d (length steps)
      Nothing -> "No path found!"

performSubs :: Set.Set Substitution -> Text.Text -> Set.Set Text.Text
performSubs subs text = Set.unions (performSub <$> Set.toList subs)
  where
    performSub Substitution{..} =
      Text.breakOnAll subFrom text
      & map (\(prefix, suffix) ->
                prefix
                <> subTo
                <> Maybe.fromMaybe suffix (Text.stripPrefix subFrom suffix)
            )
      & Set.fromList

bfs :: (Ord state, Show state) =>
  (state -> [state]) -- generate next states
  -> (state -> Bool) -- determine if answer found
  -> [(state -> Bool)] -- functions which prune on True
  -> state -- initial state
  -> Maybe [state]
bfs next pred prunes init = reverse <$> go (Map.singleton init []) Seq.empty init
  where
    go visited queue current
      | pred current = Map.lookup current visited
      | otherwise =
        let steps_so_far = Maybe.fromJust $ Map.lookup current visited
            new_states =
              Map.fromList . map (\st -> (st, st:steps_so_far)) $ next current
            new_visited = Map.unionWith shorter visited new_states
            new_queue =
              queue Seq.><
              (Seq.fromList
               . (filter (\st -> (not $ st `Map.member` visited || any ($ st) prunes)))
               $ Map.keys new_states
              )
        in pop new_queue >>= (\(x, xs) -> go new_visited xs x)
    push elem queue = queue Seq.|> elem
    pop queue = case Seq.viewl queue of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
    shorter xs ys
      | length xs <= length ys = xs
      | otherwise = ys

data Substitution = Substitution {
  subFrom :: Text.Text,
  subTo :: Text.Text
  }
  deriving(Eq, Ord)

parseInput :: Mpc.Parser (Set.Set Substitution, Text.Text)
parseInput = (,)
  <$> parseSubs
  <*> (Mpc.space *> parseSymbol <* Mpc.space)

parseSubs :: Mpc.Parser (Set.Set Substitution)
parseSubs = Set.fromList <$> parseSub `Mpc.sepEndBy` Mpc.eol
  where
    parseSub :: Mpc.Parser Substitution
    parseSub =
      Substitution
      <$> parseSymbol
      <*> (Mpc.space *> Mpc.string "=>" *> Mpc.space *> parseSymbol)

parseSymbol :: Mpc.Parser Text.Text
parseSymbol = Text.pack <$> some (Mpc.satisfy (not . Char.isSpace))

parser = argPath "input" "File from which to read input"
