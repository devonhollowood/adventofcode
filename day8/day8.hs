import Data.Char (isHexDigit, isSpace)
import Control.Monad (liftM)

main :: IO ()
main = do
    input <- liftM (filter (not . isSpace)) $ readFile "input.txt"
    print $ codeChars input - memChars input

codeChars :: String -> Int
codeChars = length

memChars :: String -> Int
memChars [] = 0
memChars ('\\':'\\':xs) = 1 + memChars xs
memChars ('\\':'\"':xs) = 1 + memChars xs
memChars ('\\':'x':y:z:xs) = if isHexDigit y && isHexDigit z
                             then 1 + memChars xs
                             else 2 + memChars (y:z:xs)
memChars ('\"':xs) = 0 + memChars xs
memChars (_:xs) = 1 + memChars xs
