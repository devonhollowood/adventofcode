{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Text.Megaparsec as Mpc
import qualified Text.Megaparsec.Text as Mpc
import qualified Text.Megaparsec.Lexer as MpcLex
import qualified Data.Text as Text
import Turtle

main :: IO ()
main = sh $ do
  filename <- options "Day 23" parser
  contents <- liftIO $ readTextFile filename
  instrs <-
    case Mpc.parse
         (parseInstructions <* Mpc.eof)
         (Text.unpack $ format fp filename)
         contents
    of
      Right val -> return val
      Left err -> die (Text.pack $ Mpc.parseErrorPretty err)
  let (a, b) = run (0, 0) instrs
  printf "Final register values (part 1):\n"
  printf ("    a: "%d%"\n") a
  printf ("    b: "%d%"\n") b

run :: (Int, Int) -> Vector Instruction -> (Int, Int)
run init instrs = go init 0
  where
    go (a, b) instr
      | instr < 0 || instr >= length instrs = (a, b)
      | otherwise =
          case instrs Vector.! instr of
            Hlf r -> go (apply (`div` 2) r) (instr + 1)
            Tpl r -> go (apply (* 3) r) (instr + 1)
            Inc r -> go (apply (+ 1) r) (instr + 1)
            Jmp o -> go (a, b) (instr + o)
            Jie r o ->
              let jmp = if even (get r) then o else 1
              in go (a, b) (instr + jmp)
            Jio r o ->
              let jmp = if get r == 1 then o else 1
              in go (a, b) (instr + jmp)
      where
        apply f A = (f a, b)
        apply f B = (a, f b)
        get A = a
        get B = b

parseInstructions :: Mpc.Parser (Vector Instruction)
parseInstructions = Vector.fromList <$> parseInstruction `Mpc.sepEndBy` Mpc.eol

parseInstruction :: Mpc.Parser Instruction
parseInstruction = Mpc.choice [
  Hlf <$> (Mpc.string "hlf " *> parseRegister),
  Tpl <$> (Mpc.string "tpl " *> parseRegister),
  Inc <$> (Mpc.string "inc " *> parseRegister),
  Jmp <$> (Mpc.string "jmp " *> parseDecimal),
  Jie
    <$> (Mpc.string "jie " *> parseRegister)
    <*> (Mpc.string ", " *> parseDecimal),
  Jio
    <$> (Mpc.string "jio " *> parseRegister)
    <*> (Mpc.string ", " *> parseDecimal)
  ]

parseDecimal :: Mpc.Parser Int
parseDecimal = fromIntegral <$>
  MpcLex.lexeme
  (pure ())
  (MpcLex.signed (pure ()) MpcLex.integer)

parseRegister :: Mpc.Parser Register
parseRegister = (Mpc.char 'a' *> pure A) <|> (Mpc.char 'b' *> pure B)

type Offset = Int

data Register = A | B

data Instruction =
  Hlf Register
  | Tpl Register
  | Inc Register
  | Jmp Offset
  | Jie Register Offset
  | Jio Register Offset

parser = argPath "instructions" "File containing instructions to run"
