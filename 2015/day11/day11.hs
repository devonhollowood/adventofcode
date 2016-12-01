main :: IO ()
main = do
    input <- filter validChar <$> readFile "input.txt"
    putStrLn . nextPassword . nextPassword $ input

nextPassword :: String -> String
nextPassword = until validPassword (getNext . increment) . getNext . increment

validPassword :: String -> Bool
validPassword pass = all ($pass) conditions
    where conditions = [hasStraight, noForbidden, doubleLets, validChars]

hasStraight :: String -> Bool
hasStraight (a:b:c:xs) = b == succ a && c == succ b || hasStraight (b:c:xs)
hasStraight _ = False

noForbidden :: String -> Bool
noForbidden s = all (`notElem` s) forbiddenChars

doubleLets :: String -> Bool
doubleLets s = countDoubles s >= 2

countDoubles :: Eq a => [a] -> Int
countDoubles (a:b:xs) = if a == b
                        then 1 + countDoubles xs
                        else countDoubles (b:xs)
countDoubles _ = 0

validChars = all validChar
validChar c = 'a' <= c && c <= 'z'

increment :: String -> Next String
increment (x:xs)
    | x `elem` forbiddenChars = (:map (const 'a') xs) <$> nextChar x
    | otherwise = case increment xs of
                      Continue ys -> Continue (x:ys)
                      Carry ys -> (:ys) <$> nextChar x
increment "" = Carry ""

forbiddenChars = "iol"

nextChar :: Char -> Next Char
nextChar c
    | c == 'z' = Carry 'a' -- Carry on 'z'
    | 'a' <= c && c <= 'y' = Continue $ succ c -- a-y: do next char
    | otherwise = Continue c -- leave other chars intact

getNext :: Next a -> a
getNext (Continue a) = a
getNext (Carry a) = a

data Next a = Continue a | Carry a deriving (Show, Eq)

instance Functor Next where
    fmap f (Continue x) = Continue (f x)
    fmap f (Carry x) = Carry (f x)
