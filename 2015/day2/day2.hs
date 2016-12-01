import Data.List (sort)
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B

main = do
    input <- B.readFile "input.txt"
    let presents =
            case parsePresents input of
                Right (p) -> p
                Left (err) -> error ("Parse error: " ++ err)
    print . sum . map required_ribbon $ presents

required_paper present = let areas = sides present in sum areas + minimum areas

sides (Present l w h) = [l*w, l*w, l*h, l*h, w*h, w*h]

required_ribbon present = smallest_perimeter present + volume present

smallest_perimeter (Present l w h) = (*2) . sum . take 2 . sort $ [l,w,h]

volume (Present l w h) = l*w*h

data Present = Present Int Int Int

parsePresents :: B.ByteString -> Either String [Present]
parsePresents = A.parseOnly (presents <* A.skipMany A.endOfLine <* A.endOfInput)

presents :: A.Parser [Present]
presents = present `A.sepBy` A.endOfLine

present :: A.Parser Present
present = do
    l <- A.decimal
    A.char 'x'
    w <- A.decimal
    A.char 'x'
    h <- A.decimal
    return $ Present l w h
