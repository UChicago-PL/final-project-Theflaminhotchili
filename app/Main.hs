module Main where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import Data.Functor.Contravariant (Equivalence)

data Expr
  = Num Double
  | Var String
  | Neg Expr
  | BinOp Op Expr Expr
  deriving (Show, Eq)

data Eqn = Eqn Expr Expr
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

eqn :: ReadP Eqn
eqn = do
    lhs <- expr
    skipSpaces
    _ <- char '='
    skipSpaces
    Eqn lhs <$> expr
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

parseInput :: String -> Either Expr Eqn
parseInput input
  | '=' `elem` input = case filter (null . snd) (readP_to_S eqn input) of
                       [(result,_)] -> Right result
                       _ -> error "BadArgument"
  | otherwise = case filter (null . snd) (readP_to_S expr input) of
                [(result,_)] -> Left result
                _ -> error "BadArgument"


-- ======================================= --
-- ======== EXPRESSION EVALUATION ======== --
-- ======================================= --

evalExpr :: Expr -> Double
evalExpr e = case e of
 BinOp Add g f -> (evalExpr g) + (evalExpr f)
 BinOp Sub g f -> (evalExpr g) - (evalExpr f)
 BinOp Mul g f -> (evalExpr g) * (evalExpr f)
 BinOp Div g f -> (evalExpr g) / (evalExpr f)
 BinOp Exp g f -> (evalExpr g) ** (evalExpr f)
 Neg n -> (-1 * (evalExpr n))
 Var _ -> 1.0
 Num n -> n