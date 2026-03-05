module Main where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)

data Expr
  = Num Double
  | Var String
  | Neg Expr
  | BinOp Op Expr Expr
  deriving (Show, Eq)

data Op = Add | Sub | Mul | Div | Exp
  deriving (Show, Eq)

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- ================================== --
-- ======== PARSER FUNCTIONS ======== --
-- ================================== --

--Read operations
addOp :: ReadP (Expr -> Expr -> Expr)
addOp = (char '+' >> return (BinOp Add)) +++ (char '-' >> return (BinOp Sub))
mulOp :: ReadP (Expr -> Expr -> Expr)
mulOp = (char '*' >> return (BinOp Mul)) +++ (char '/' >> return (BinOp Div))
expOp :: ReadP (Expr -> Expr -> Expr)
expOp = char '^' >> return (BinOp Exp)
numOp :: ReadP (Expr)
numOp = do
  numInt <- many1 (satisfy isDigit)
  return (Num (read numInt))
varOp :: ReadP (Expr)
varOp = do
  varName <- many1 (satisfy isAlpha)
  return (Var (varName))


expr :: ReadP Expr
expr = chainl1 term addOp
term :: ReadP Expr
term = chainl1 power mulOp
power :: ReadP Expr
power = chainr1 sign expOp
sign :: ReadP Expr
sign = (char '-' >> sign >>= return . Neg) <++ factor
factor :: ReadP Expr
factor = between (char '(')  (char ')') expr
  <++ numOp
  <++ varOp

parseInput :: String -> Expr
parseInput input = case filter (null . snd) (readP_to_S expr input) of
  [(result,_)] -> result
  _ -> error "BadArgument"


-- ======================================= --
-- ======== EXPRESSION EVALUATION ======== --
-- ======================================= --