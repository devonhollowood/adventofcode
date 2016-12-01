import Data.Char (isHexDigit)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let encoded = concatMap encode . lines $ input
    let unencoded = concat . lines $ input
    print $ codeChars encoded - codeChars unencoded

encode :: String -> String
encode s = "\"" ++ concatMap escape s ++ "\""
    where escape '\"' = "\\\""
          escape '\\' = "\\\\"
          escape x = [x]

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
