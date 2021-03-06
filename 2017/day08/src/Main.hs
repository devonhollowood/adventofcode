{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as Mpc
import qualified Text.Megaparsec.Lexer as MpcLex
import qualified Text.Megaparsec.Text as Mpc

import Data.Either (either)
import Data.Foldable (maximumBy)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Control.Applicative ((<|>))
import Data.Semigroup ((<>))

main :: IO ()
main = do
  filename <- Opt.execParser optPath
  contents <- Text.IO.readFile filename
  let instructions =
        either (error . Mpc.parseErrorPretty) id
        $ Mpc.runParser pInstructions filename contents
  let update_with_max (old_st, best@(_, best_val)) inst =
        case perform old_st inst of
          Nothing -> (old_st, best)
          change@(Just new@(_, val)) ->
            if val > best_val
            then (update old_st change, new)
            else (update old_st change, best)
  let (final_st, highest) =
        foldl' update_with_max (Map.empty, ("(none)", 0)) instructions
  let max_entry = maximumBy (comparing snd) (Map.assocs final_st)
  putStrLn $ "Part 1: " ++ show (fst max_entry) ++ " = " ++ show (snd max_entry)
  putStrLn $ "Part 2: " ++ show (fst highest) ++ " = " ++ show (snd highest)

deref :: CpuState -> Operand -> Int
deref _ (OpNum i) = i
deref state (OpAddr addr) = fromMaybe 0 $ Map.lookup addr state

update :: CpuState -> Maybe (Address, Int) -> CpuState
update state (Just (addr, val)) = Map.insert addr val state
update state Nothing = state

perform :: CpuState -> Instruction -> Maybe (Address, Int)
perform state (Instruction Action{..} Conditional{..}) =
  if condOp state condLeft condRight
  then Just $ actOp state actLeft actRight
  else Nothing

raiseOp :: (Int -> Int -> a) -> CpuState -> Operand -> Operand -> a
raiseOp f state opA opB = f (deref state opA) (deref state opB)

mkAct :: (Int -> Int -> Int) -> CpuState -> Address -> Operand -> (Address, Int)
mkAct f state left right =
    (left, f (deref state $ OpAddr left) (deref state right))

data Instruction = Instruction {
  instAct :: Action,
  instCond :: Conditional
}

data Action = Action {
  actOp :: CpuState -> Address -> Operand -> (Address, Int),
  actLeft :: Address,
  actRight :: Operand
}

data Conditional = Conditional {
  condOp :: CpuState -> Operand -> Operand -> Bool,
  condLeft :: Operand,
  condRight :: Operand
}

data Operand = OpAddr Address | OpNum Int
type CpuState = Map.Map Address Int
type Address = Text.Text

pInstructions :: Mpc.Parser [Instruction]
pInstructions = pInstruction `Mpc.sepEndBy` Mpc.eol

pInstruction :: Mpc.Parser Instruction
pInstruction = Instruction <$> pAction <*> (Mpc.char ' ' *> pConditional)

pAction :: Mpc.Parser Action
pAction = do
  left <- pAddr
  _ <- Mpc.char ' '
  act <-
    (Mpc.string "dec" *> pure (mkAct (-)))
    <|> (Mpc.string "inc" *> pure (mkAct (+)))
  _ <- Mpc.char ' '
  right <- pOperand
  pure $ Action act left right

pConditional :: Mpc.Parser Conditional
pConditional = do
  _ <- Mpc.string "if "
  left <- pOperand
  _ <- Mpc.char ' '
  op <-
    (Mpc.string "<=" *> pure (raiseOp (<=)))
    <|> (Mpc.string "<" *> pure (raiseOp (<)))
    <|> (Mpc.string "==" *> pure (raiseOp (==)))
    <|> (Mpc.string "!=" *> pure (raiseOp (/=)))
    <|> (Mpc.string ">=" *> pure (raiseOp (>=)))
    <|> (Mpc.string ">" *> pure (raiseOp (>)))
  _ <- Mpc.char ' '
  right <- pOperand
  pure $ Conditional op left right

pOperand :: Mpc.Parser Operand
pOperand =
  (OpAddr <$> pAddr)
  <|> (OpNum . fromIntegral <$> MpcLex.signed Mpc.space MpcLex.integer)

pAddr :: Mpc.Parser Address
pAddr = Text.pack <$> Mpc.some Mpc.letterChar

optPath :: Opt.ParserInfo FilePath
optPath =
  Opt.info
  (Opt.strArgument (
        Opt.metavar "INPUT"
        <> Opt.help "Input file"
      )
    Opt.<**> Opt.helper
  )
  Opt.fullDesc
