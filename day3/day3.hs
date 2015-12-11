import Data.List (nub)

main = do
    input <- readFile "input.txt"
    let directions = map toDir . filter (`elem` "^v<>") $ input
    let houses = applyDirections (0,0) directions
    let unique_houses = length . nub $ houses
    print unique_houses

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
