import Data.List (isInfixOf)

main = do
    input <- readFile "input.txt"
    print . length . filter isNice . map classify $ lines input

classify :: String -> Classification
classify s
    | all ($s) tests = Nice
    | otherwise = Naughty
    where tests = [sandwich, doubleRepeat]

sandwich (x:y:z:xs) = x == z || sandwich (y:z:xs)
sandwich _ = False

doubleRepeat (x:y:xs) = [x,y] `isInfixOf` xs || doubleRepeat (y:xs)
doubleRepeat _ = False

threeVowels = (>=3) . length . filter (`elem` "aeiou")

doubleLetter (x:y:xs) = x==y || doubleLetter (y:xs)
doubleLetter _ = False

noForbidden s = not $ any (`isInfixOf` s) forbidden
    where forbidden = ["ab", "cd", "pq", "xy"]

isNice :: Classification -> Bool
isNice c = c == Nice

data Classification = Naughty | Nice deriving (Eq, Show)
