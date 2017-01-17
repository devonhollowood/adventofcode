{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import qualified Text.Megaparsec as Mpc
import qualified Text.Megaparsec.Text as Mpc
import qualified Data.Text as Text
import qualified Data.Set as Set
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

data Substitution = Substitution {
  subFrom :: Text.Text,
  subTo :: Text.Text
  }
  deriving(Eq, Ord)

parseInput :: Mpc.Parser (Set.Set Substitution, Text.Text)
parseInput = (,)
  <$> parseSubs
  <*> (Mpc.space *> fmap Text.pack (many Mpc.anyChar))

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
