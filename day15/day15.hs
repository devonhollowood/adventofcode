{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
    ingrs <- parseAssert pIngredients <$> B.readFile "input.txt"
    print . maximum . map score . combinations 100 $ ingrs

score :: [(Int, Ingredient)] -> Score
score amounts =
    let Properties{..} = combine amounts in
    capacity*durability*flavor*texture

combine :: [(Int, Ingredient)] -> Properties
combine = regularize . foldr (propertyAdd . mult) (Properties 0 0 0 0 0)
    where mult (amt, ingr) = amt `propertyMult` properties ingr

infixl 6 `propertyAdd`
propertyAdd :: Properties -> Properties -> Properties
propertyAdd (Properties c1 d1 f1 t1 l1) (Properties c2 d2 f2 t2 l2) =
    Properties (c1 + c2) (d1 + d2) (f1 + f2) (t1 + t2) (l1 + l2)

infixl 7 `propertyMult`
propertyMult :: Int -> Properties -> Properties
propertyMult n (Properties c d f t l) = Properties (n*c) (n*d) (n*f) (n*t) (n*l)

regularize :: Properties -> Properties
regularize (Properties c d f t l) =
    Properties (reg c) (reg d) (reg f) (reg t) (reg l)
    where reg n | n > 0 = n | otherwise = 0

combinations :: Int -> [a] -> [[(Int, a)]]
combinations _ [] = [[]]
combinations n [ingr] = [[(n, ingr)]]
combinations n (ingr:ingrs) = concatMap go [0..n]
    where go q = map ((q, ingr):) $ combinations (n-q) ingrs

pIngredients :: Parser [Ingredient]
pIngredients = pIngredient `sepBy` endOfLine <* ending
    where ending = many endOfLine <* endOfInput

pIngredient :: Parser Ingredient
pIngredient = Ingredient <$> many (notChar ':') <* ": " <*> pProperties

pProperties :: Parser Properties
pProperties = Properties <$>
    ("capacity " *> signed decimal <* ", ") <*>
    ("durability " *> signed decimal <* ", ") <*>
    ("flavor " *> signed decimal <* ", ") <*>
    ("texture " *> signed decimal <* ", ") <*>
    ("calories " *> signed decimal)

data Ingredient = Ingredient {
    name :: String,
    properties :: Properties
} deriving (Show)

data Properties = Properties {
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
} deriving (Show)

type Score = Int

parseAssert :: Parser a -> B.ByteString -> a
parseAssert parser input =
    case parseOnly parser input of
        Right p -> p
        Left err -> error $ "Bad parse: " ++ err
