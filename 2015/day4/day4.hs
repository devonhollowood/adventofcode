import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (find, isPrefixOf)
import Data.Char (isAlphaNum)

main = do
    input <- readFile "input.txt"
    let key = BC.pack $ filter isAlphaNum input
    print $ find (valid . tryNum key) [1..]


valid :: String -> Bool
valid = ("000000" `isPrefixOf`)

tryNum :: Key -> Int -> String
tryNum key num = show . md5 $ key `BL.append` BC.pack (show num)

type Key = BL.ByteString
