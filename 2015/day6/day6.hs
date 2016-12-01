{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative ((<|>))
import Control.Monad.ST (runST)
import System.Exit (die)

main :: IO ()
main = do
    input <- B.readFile "input.txt"
    instrs <- case parseInput input of
                   Right is -> return is
                   Left err -> die err
    let lights = setUpLights instrs
    print . V.sum . V.map brightness $ lights

nrows :: Int
nrows = 1000

ncols :: Int
ncols = 1000

setUpLights :: [Instruction] -> Lights
setUpLights instrs = runST $ do
    mv <- MV.replicate (nrows*ncols) (Light 0)
    mapM_ (perform mv) instrs
    V.freeze mv
    where perform mv instr =
              MV.modify mv (action instr) (flatten ncols $ coord instr)

getSquare :: Coordinate -> Coordinate -> [Coordinate]
getSquare (r1, c1) (r2, c2) = [(a, b) | a <- [r1..r2], b <- [c1..c2]]

turnOn :: Action
turnOn (Light b)  = Light $ b+1

turnOff :: Action
turnOff (Light b) = Light $ max 0 (b-1)

toggle :: Action
toggle (Light b) = Light $ b+2

flatten :: Int -> Coordinate -> Int -- Translate Coordinate -> Vector Position
flatten numcols (r,c) = r*numcols + c

data Instruction = Instruction {
    action :: Action,
    coord :: Coordinate
}
type Action = Light -> Light
type Lights = V.Vector Light
type Coordinate = (Int, Int)
data Light = Light {
    brightness:: Brightness
}
type Brightness = Int

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

pCoord :: Parser Coordinate
pCoord = do
    row <- decimal
    char ','
    col <- decimal
    pure (row, col)
