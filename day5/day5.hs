import Data.List (isInfixOf)

main = do
    input <- readFile "input.txt"
    print . length . filter isNice . map classify $ lines input

classify :: String -> Classification
classify s
    | all ($s) tests = Nice
    | otherwise = Naughty
    where tests = [three_vowels, double_letter, no_forbidden]

three_vowels = (>=3) . length . filter (`elem` "aeiou")

double_letter (x:y:xs) = x==y || double_letter (y:xs)
double_letter _ = False

no_forbidden s = not $ any (`isInfixOf` s) forbidden
    where forbidden = ["ab", "cd", "pq", "xy"]

isNice :: Classification -> Bool
isNice c = c == Nice

data Classification = Naughty | Nice deriving (Eq, Show)
