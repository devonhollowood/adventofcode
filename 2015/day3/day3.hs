import Data.List (nub)

main = do
    input <- readFile "input.txt"
    let directions = map toDir . filter (`elem` "^v<>") $ input
    let (santa_dir, robo_dir) = splitDirections directions
    let santa_houses = applyDirections (0,0) santa_dir
    let robo_houses = applyDirections (0,0) robo_dir
    let houses = santa_houses ++ robo_houses
    let unique_houses = length . nub $ houses
    print unique_houses

splitDirections :: [Direction] -> ([Direction], [Direction])
splitDirections (x1:x2:xs) =
    let (x1s, x2s) = splitDirections xs
    in (x1:x1s, x2:x2s)
splitDirections [x] = ([x], [])
splitDirections []  = ([], [])

applyDirections :: House -> [Direction] -> [House]
applyDirections = scanl nextHouse

toDir '^' = North
toDir 'v' = South
toDir '>' = East
toDir '<' = West

nextHouse :: House -> Direction -> House
nextHouse (i,j) dir =
    case dir of
        North -> (i, j+1)
        South -> (i, j-1)
        East  -> (i+1, j)
        West  -> (i-1, j)

data Direction = North | South | East | West
type House = (Int, Int)
