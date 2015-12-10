import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B

main = do
    input <- B.readFile "input.txt"
    let presents =
            case parsePresents input of
                Right (p) -> p
                Left (err) -> error ("Parse error: " ++ err)
    print . sum . map required_paper $ presents

required_paper present = let areas = sides present in sum areas + minimum areas

sides (Present l w h) = [l*w, l*w, l*h, l*h, w*h, w*h]

data Present = Present Int Int Int

parsePresents :: B.ByteString -> Either String [Present]
parsePresents = parseOnly (presents <* skipMany endOfLine <* endOfInput)

presents :: Parser [Present]
presents = present `sepBy` endOfLine

present :: Parser Present
present = do
    l <- decimal
    char 'x'
    w <- decimal
    char 'x'
    h <- decimal
    return $ Present l w h
