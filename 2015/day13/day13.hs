{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import KeyGraph
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.String (IsString)
import Data.Maybe (fromMaybe)
import Data.List (permutations)

main :: IO ()
main = do
    input <- B.readFile "input.txt"
    let prefs = case parseOnly pPreferences input of
                    Right ps -> ps
                    Left err -> error err
    let prefs' = addNode prefs "Devon"
    print $ getBest prefs'

getBest :: Preferences -> Desire
getBest prefs = maximum . map (score prefs) . permutations $ nodes prefs

score :: Preferences -> [Guest] -> Desire
score prefs = sum . map total_change . pairs
    where total_change (g1, g2) = change (g1, g2) + change (g2, g1)
          change (g1, g2) = fromMaybe 0 $ edge prefs g1 g2
          pairs [] = []
          pairs xs = (last xs, head xs) : zip xs (tail xs)

pPreferences :: Parser Preferences
pPreferences = fromList <$> pPreference `sepBy` endOfLine <* ending
    where ending = many endOfLine <* endOfInput

pPreference :: Parser (Guest, Guest, Desire)
pPreference = (\g1 d g2 -> (g1, g2, d)) <$> pGuest <* " would " <*> pChange <*
              " happiness units by sitting next to " <*> pGuest <* "."

pGuest :: Parser Guest
pGuest = Guest <$> many letter_ascii

pChange :: Parser Desire
pChange = (id <$ "gain" <|> negate <$ "lose") <* " " <*> decimal

type Preferences = KeyGraph Guest Desire
newtype Guest = Guest String deriving (Eq, Ord, Show, IsString)
type Desire = Int
