import Data.List (group)
import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
    input <- map digitToInt . filter isDigit <$> readFile "input.txt"
    print . length . (!! 40) . iterate lookAndSay $ input

lookAndSay :: [Int] -> [Int]
lookAndSay = concatMap say . group
    where say xss@(x:_) = [length xss, x]
          say [] = error "Empty say!"
