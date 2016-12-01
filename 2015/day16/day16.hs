{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Control.Applicative

main :: IO ()
main = do
    sues <- parseAssert pSues <$> B.readFile "input.txt"
    print . filter (`matches` target) $ sues

matches :: Sue -> M.Map Attribute Int -> Bool
matches Sue{..} m = all (attrmatch m) attributes
    where attrmatch m (attr, num)
            | attr `elem` ["cats", "trees"] =
                maybe True (< num) (M.lookup attr m)
            | attr `elem` ["pomeranians", "goldfish"] =
                maybe True (> num) (M.lookup attr m)
            | otherwise =
                maybe True (== num) (M.lookup attr m)

pSues :: Parser [Sue]
pSues = pSue `sepBy` endOfLine <* ending
    where ending = many endOfLine <* endOfInput

pSue :: Parser Sue
pSue = Sue <$> ("Sue " *> decimal) <* ": " <*> pAttribute `sepBy` ", "

pAttribute :: Parser (Attribute, Int)
pAttribute = (,) <$> many (notChar ':') <* ": " <*> decimal

target :: M.Map Attribute Int
target = M.fromList [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)
    ]

data Sue = Sue {
    idNum :: Int,
    attributes :: [(Attribute, Int)]
} deriving (Show)

type Attribute = String

parseAssert :: Parser a -> B.ByteString -> a
parseAssert parser input =
    case parseOnly parser input of
        Right p -> p
        Left err -> error $ "Bad parse: " ++ err
