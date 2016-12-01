main = do
    input <- readFile "input.txt"
    print $ findBasement input

countParens = foldr count 0

findBasement = length . takeWhile (>= 0) . scanl (flip count) 0

count '(' n = n+1
count ')' n = n-1
count _   n = n
