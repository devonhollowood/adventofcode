{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Array as Array
import Turtle
import qualified Text.Megaparsec as Mpc
import qualified Text.Megaparsec.Text as Mpc
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe

main :: IO ()
main = sh $ do
  (fname, n_iter) <- options "Day 18" parser
  contents <- liftIO $ readTextFile fname
  initial_grid <-
        case Mpc.parse (parseGrid <* Mpc.eof)
             (Text.unpack $ format fp fname) contents of
          Left parse_err -> die $ Text.pack (Mpc.parseErrorPretty parse_err)
          Right grid -> return grid
  let final_grid = iterate nextGrid initial_grid !! n_iter
  printf w final_grid
  printf ("Number of \"on\" lights (Part 1): "%d%"\n") (gridOn final_grid)
  let final_grid2 =
        iterate (fixCorners . nextGrid) (fixCorners initial_grid) !! n_iter
  printf w final_grid2
  printf ("Number of \"on\" lights (Part 2): "%d%"\n") (gridOn final_grid2)

newtype Grid = Grid {
  lights :: Array.Array (Int, Int) Light
  }

instance Show Grid where
  show grid@Grid{..} =
    unlines [
      concat [show $ lights Array.! (r, c) | c <- [0 .. gridWidth grid - 1]]
      | r <- [0 .. gridHeight grid - 1]
      ]

data Light = On | Off
  deriving (Eq)

instance Show Light where
  show On = "#"
  show Off = "."

isOn :: Light -> Bool
isOn light =
  case light of
    On -> True
    Off -> False

gridWidth :: Grid -> Int
gridWidth = (+1) . fst . snd . Array.bounds . lights

gridHeight :: Grid -> Int
gridHeight = (+1) . snd . snd . Array.bounds . lights

gridOn :: Grid -> Int
gridOn = length . filter isOn . Array.elems . lights

at :: Grid -> (Int, Int) -> Maybe Light
Grid{..} `at` pos
  | Array.inRange (Array.bounds lights) pos = Just $ lights Array.! pos
  | otherwise = Nothing

nextGrid :: Grid -> Grid
nextGrid grid@Grid{..} = Grid $ iemap updateLight lights
  where
    updateLight pos light =
      case light of
        On -> if on_neighbors pos `elem` [2, 3] then On else Off
        Off -> if on_neighbors pos == 3 then On else Off
    on_neighbors (r, c) =
      length
      . filter isOn
      . Maybe.mapMaybe (at grid)
      $ [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1),
         (r, c - 1),                 (r, c + 1),
         (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]

fixCorners :: Grid -> Grid
fixCorners grid =
  let arr = lights grid
      lastRow = gridHeight grid - 1
      lastCol = gridWidth grid - 1
      corners = [(0, 0), (0, lastCol), (lastRow, 0), (lastRow, lastCol)]
  in Grid $ arr Array.// map (\pos -> (pos, On)) corners

iemap :: Array.Ix i => (i -> e -> e) -> Array.Array i e -> Array.Array i e
iemap fn arr = Array.array (Array.bounds arr) (map update $ Array.assocs arr)
  where
    update (ix, el) = (ix, fn ix el)

parseGrid :: Mpc.Parser Grid
parseGrid = do
  rows <- some parseLight `Mpc.sepEndBy1` Mpc.eol
  let width = length (head rows)
  let height = length rows
  return . Grid $ Array.listArray ((0, 0), (width - 1, height - 1)) (concat rows)

parseLight :: Mpc.Parser Light
parseLight = (Mpc.char '#' *> return On) <|> (Mpc.char '.' *> return Off)

parser = (,)
  <$> argPath "input" "File from which to read input"
  <*> (fromIntegral . Maybe.fromMaybe 100
       <$> optional (optInteger "n-iter" 'n' "Number of iterations to run")
      )
