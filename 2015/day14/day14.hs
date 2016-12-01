{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString as B
import Control.Applicative
import Data.List (sortBy, groupBy, maximumBy, transpose)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.MultiSet as MS

main :: IO ()
main = do
    deer <- parseAssert pHerd <$> B.readFile "input.txt"
    let time = 2503
    print . snd . maxOccur $ scores time deer

maxOccur :: Ord a => MS.MultiSet a -> (a, Int)
maxOccur = maximumBy (comparing snd) . MS.toOccurList

scores :: Time -> [Reindeer] -> MS.MultiSet Reindeer
scores time deer = MS.fromList . concat . take time . zipManyWith best $ deer_dists
    where best = map fst . last . groupBy ((==) `on` snd) . sortBy (comparing snd)
          best :: [(Reindeer, Distance)] -> [Reindeer]
          deer_dists = map single_dists deer
          deer_dists :: [[(Reindeer, Distance)]]
          single_dists d = map ((,) d) $ distances d
          single_dists :: Reindeer -> [(Reindeer, Distance)]

zipManyWith :: ([a] -> b) -> [[a]] -> [b]
zipManyWith f = map f . transpose

distances :: Reindeer -> [Distance]
distances = tail . scanl (+) 0 . gains

traveled :: Time -> Reindeer -> Distance
traveled t = sum . take t . gains

gains :: Reindeer -> [Distance]
gains Reindeer{..} = cycle $ replicate flyTime speed ++ replicate restTime 0

pHerd :: Parser [Reindeer]
pHerd = pReindeer `sepBy` endOfLine <* ending
    where ending = many endOfLine <* endOfInput

pReindeer :: Parser Reindeer
pReindeer = Reindeer <$>
    pName <* " can fly " <*>
    decimal <* " km/s for " <*>
    decimal <* " seconds, but then must rest for " <*>
    decimal <* " seconds."
    where pName = many letter_ascii

data Reindeer = Reindeer {
    name :: String,
    speed :: Int,
    flyTime :: Int,
    restTime :: Int
} deriving (Eq, Show, Ord)

type Distance = Int
type Time = Int

parseAssert :: Parser a -> B.ByteString -> a
parseAssert parser input =
    case parseOnly parser input of
        Right p -> p
        Left err -> error $ "Bad parse: " ++ err

