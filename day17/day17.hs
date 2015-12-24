import Data.List (sort, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)

main = do
    containers <- map (read :: String -> Int) . lines <$> readFile "input.txt"
    let ascending = sort containers
    print . length . head . groupBy ((==) `on` length)
          . sortBy (comparing length) . ascCombos 150
          $ ascending

-- Ways to fill `size` from ascending list
ascCombos :: Int -> [Int] -> [[Int]]
ascCombos _ [] = [] -- Can't fill anything from an empty list
ascCombos size (x:xs)
    | x < size  = map (x:) (ascCombos (size-x) xs) ++ --using this element
                  ascCombos size xs        --skipping this element
    | x == size = [x] : ascCombos size xs
    | otherwise = [] -- `x` too big, can't fill with it
