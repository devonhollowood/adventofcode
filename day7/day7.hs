{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import System.Exit (die)
import Data.Char (isLower)
import Data.Bits
import qualified Data.Map as M

main :: IO ()
main = do
    input <- B.readFile "input.txt"
    wire_map <- case parseInput input of
                  Right ws -> return ws
                  Left err -> die err
    print $ resolve wire_map (VariableRef $ Variable "a")

resolve :: ExprTable -> Expression -> Maybe Int
resolve exprs expr = evalStateT (resolveExpr exprs expr) M.empty

resolveExpr :: ExprTable -> Expression -> StateT ValTable Maybe Int
resolveExpr exprs (BinaryGate gate ida idb) = do
    a <- resolveExpr exprs ida
    b <- resolveExpr exprs idb
    return $ binaryGateFunc gate a b
resolveExpr exprs (UnaryGate gate ida) =
    unaryGateFunc gate <$> resolveExpr exprs ida
resolveExpr exprs (VariableRef var) = do
    cache <- get
    case M.lookup var cache of
        Just res -> lift res
        Nothing -> do
            res <- lift (M.lookup var exprs) >>= resolveExpr (M.delete var exprs)
            modify $ M.insert var (Just res)
            return res
resolveExpr _ (Literal n) = return n

binaryGateFunc :: BinaryGateType -> Int -> Int -> Int
binaryGateFunc And = (.&.)
binaryGateFunc Or = (.|.)
binaryGateFunc Lshift = shiftL
binaryGateFunc Rshift = shiftR

unaryGateFunc :: UnaryGateType -> Int -> Int
unaryGateFunc Not = complement

parseInput :: B.ByteString -> Either String (M.Map Variable Expression)
parseInput = parseOnly $ M.fromList <$> pWire `sepBy` endOfLine <* ending
    where ending = many endOfLine *> endOfInput

pWire :: Parser Assignment
pWire = flip (,) <$> pExpression <*> (string " -> " *> pVariable)

pExpression :: Parser Expression
pExpression = pUnaryGate <|> pBinaryGate <|> pVariableRef <|> pLiteral

pBinaryGate :: Parser Expression
pBinaryGate =
    (\wa gate wb -> BinaryGate gate wa wb) <$>
        pSimpleExpression <*>
        (char ' ' *> pBinaryGateType <* char ' ') <*>
        pSimpleExpression

pUnaryGate :: Parser Expression
pUnaryGate =
    UnaryGate <$> pUnaryGateType <* char ' ' <*> pSimpleExpression

pSimpleExpression :: Parser Expression
pSimpleExpression = pVariableRef <|> pLiteral

pVariableRef :: Parser Expression
pVariableRef = VariableRef <$> pVariable

pLiteral :: Parser Expression
pLiteral = Literal <$> decimal

pVariable :: Parser Variable
pVariable = Variable <$> pVariableName

pVariableName :: Parser VariableName
pVariableName = some $ satisfy isLower

pBinaryGateType :: Parser BinaryGateType
pBinaryGateType =
    string "AND" *> pure And <|>
    string "OR" *> pure Or <|>
    string "LSHIFT" *> pure Lshift <|>
    string "RSHIFT" *> pure Rshift

pUnaryGateType :: Parser UnaryGateType
pUnaryGateType =
    string "NOT" *> pure Not

type ExprTable = M.Map Variable Expression
type ValTable = M.Map Variable (Maybe Int)

type Assignment = (Variable, Expression)

data Expression = BinaryGate BinaryGateType Expression Expression
           | UnaryGate UnaryGateType Expression
           | VariableRef Variable
           | Literal Int
           deriving Show

data Variable = Variable VariableName deriving (Eq, Show, Ord)

data BinaryGateType = And | Or | Lshift | Rshift deriving Show

data UnaryGateType = Not deriving Show

type VariableName = String
