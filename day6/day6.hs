{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative ((<|>))
import Control.Monad.ST (runST)
import System.Exit (die)

main = do
    input <- B.readFile "input.txt"
    instrs <- case parseInput input of
                   Right is -> return is
                   Left err -> die err
    let lights = setUpLights instrs
    print . V.length $ V.filter isLit lights

nrows = 1000
ncols = 1000

setUpLights :: [Instruction] -> Lights
setUpLights instrs = runST $ do
    mv <- MV.replicate (nrows*ncols) Unlit
    mapM_ (perform mv) instrs
    V.freeze mv
    where perform mv instr =
              MV.modify mv (action instr) (flatten ncols $ coord instr)

getSquare :: Coordinate -> Coordinate -> [Coordinate]
getSquare (r1, c1) (r2, c2) = [(a, b) | a <- [r1..r2], b <- [c1..c2]]

turnOn light = Lit
turnOff light = Unlit
toggle Lit = Unlit
toggle Unlit = Lit

flatten :: Int -> Coordinate -> Int -- Translate Coordinate -> Vector Position
flatten ncols (r,c) = r*ncols + c

unflatten :: Int -> Int -> Coordinate -- Translate Vector Position -> Coordinate
unflatten ncols pos = pos `divMod` ncols

isLit light = light == Lit

data Instruction = Instruction {
    action :: Action,
    coord :: Coordinate
}
type Action = Light -> Light
type Lights = V.Vector Light
type Coordinate = (Int, Int)
data Light = Lit | Unlit deriving (Eq, Show)

parseInput :: B.ByteString -> Either String [Instruction]
parseInput = parseOnly $ concat <$> pInstructions `sepBy` endOfLine <* ending
    where ending = option () endOfLine <* endOfInput

pInstructions :: Parser [Instruction]
pInstructions = do
    act <- pAction
    char ' '
    ul <- pCoord
    string " through "
    br <- pCoord
    let instrs = map (Instruction act) $ getSquare ul br
    pure instrs

pAction :: Parser Action
pAction = pOn <|> pOff <|> pToggle
    where pOn = string "turn on" *> pure turnOn
          pOff = string "turn off" *> pure turnOff
          pToggle = string "toggle" *> pure toggle

pCoord = do
    row <- decimal
    char ','
    col <- decimal
    pure (row, col)
