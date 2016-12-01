{-# LANGUAGE OverloadedStrings #-}

import KeyGraph
import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    input <- B.readFile "input.txt"
    let world = case parseOnly pWorld input of
                    Right w -> w
                    Left err -> error err
    print $ shortestTrip world

shortestTrip :: World -> Maybe Distance
shortestTrip w = listSafe maximum . map (bestTripFrom w) $ nodes w

bestTripFrom :: World -> Location -> Distance
bestTripFrom w l = fromMaybe 0 (edgesFrom w l >>= bestOfEdges w l)

bestOfEdges :: World -> Location -> [(Location, Distance)] -> Maybe Distance
bestOfEdges w from = listSafe maximum . map (get_trip_len subw)
    where subw = deleteUndirected w from
          get_trip_len w' (dest, dist)
              | null w' = 0
              | otherwise = dist + bestTripFrom w' dest

listSafe :: ([a] -> b) -> [a] -> Maybe b
listSafe f xs
    | null xs = Nothing
    | otherwise = Just $ f xs

pWorld :: Parser World
pWorld = fromListUndirected <$> pVoyage `sepBy` endOfLine <* ending
    where ending = many endOfLine <* endOfInput


pVoyage :: Parser (Location, Location, Distance)
pVoyage = (,,) <$> pLocation <* " to " <*> pLocation <* " = " <*> decimal

pLocation :: Parser Location
pLocation = many $ satisfy (not . isSpace)

type World = KeyGraph Location Distance
type Distance = Int
type Location = String
