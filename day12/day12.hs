import qualified Data.ByteString as B
import Data.Aeson
import Data.Scientific
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    input <- B.readFile "input.txt"
    let decoded = fromMaybe (error "Could not parse") $ decodeStrict input
    print $ addDigits decoded

addDigits :: Value -> Scientific
addDigits (Object o) = sum . map addDigits $ HM.elems o
addDigits (Array a) = sum . fmap addDigits $ a
addDigits (Number n) = n
addDigits _ = 0
