main = do
    input <- readFile "input.txt"
    print $ countParens input

countParens = foldr count 0
    where count '(' n = n+1
          count ')' n = n-1
          count _   n = n
