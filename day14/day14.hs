{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString as B
import Control.Applicative

main :: IO ()
main = do
    deer <- parseAssert pHerd <$> B.readFile "input.txt"
    let time = 2503
    print . maximum . map (traveled time) $ deer

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
}

type Distance = Int
type Time = Int

parseAssert :: Parser a -> B.ByteString -> a
parseAssert parser input =
    case parseOnly parser input of
        Right p -> p
        Left err -> error $ "Bad parse: " ++ err

