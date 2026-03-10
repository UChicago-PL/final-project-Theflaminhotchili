module Main where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import Data.Either

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

epsilon :: Double
epsilon = 1e-10

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- ================================== --
-- ======== PARSER FUNCTIONS ======== --
-- ================================== --

--Read operations
addOp :: ReadP (Expr -> Expr -> Expr)
addOp =
  (skipSpaces >> char '+' >> return (BinOp Add)) +++
  (skipSpaces >> char '-' >> return (BinOp Sub))
mulOp :: ReadP (Expr -> Expr -> Expr)
mulOp =
  (skipSpaces >> char '*' >> return (BinOp Mul)) +++
  (skipSpaces >> char '/' >> return (BinOp Div))
expOp :: ReadP (Expr -> Expr -> Expr)
expOp = skipSpaces >> char '^' >> return (BinOp Exp)
numOp :: ReadP (Expr)
numOp = do
  numInt <- many1 (satisfy isDigit)
  skipSpaces
  v <- option Nothing (fmap Just (many1 (satisfy isAlpha)))
  case v of --For cases like 3x -> 3*x
    Just var -> return (BinOp Mul (Num (read numInt)) (Var var))
    Nothing -> return (Num (read numInt))
varOp :: ReadP (Expr)
varOp = do
  skipSpaces
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
  <++  numOp
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

evalAt :: String -> Double -> Expr -> Double
evalAt v val e = case e of
  BinOp Add f g -> (evalAt v val f) + (evalAt v val g)
  BinOp Sub f g -> (evalAt v val f) - (evalAt v val g)
  BinOp Mul f g -> (evalAt v val f) * (evalAt v val g)
  BinOp Div f g -> (evalAt v val f) / (evalExpr g)
  BinOp Exp f g -> (evalAt v val f) ** (evalAt v val g)
  Neg n -> (-1 * (evalAt v val n))
  Var v'
    | v' == v -> val
    | otherwise -> 1.0
  Num n -> n


diffExpr :: String -> Expr -> Expr --Differentiate expr for newton's method
diffExpr v e = case e of --Idk why the indentation is like that i tried fixing it and it didnt work
 BinOp Add f g -> BinOp Add (diffExpr v f) (diffExpr v g)
 BinOp Sub f g -> BinOp Sub (diffExpr v f) (diffExpr v g)
 BinOp Mul f g -> --Product Rule
  BinOp Add (BinOp Mul (diffExpr v g) f) (BinOp Mul (diffExpr v f) g)
 BinOp Div f g -> --Quotient Rule
  BinOp Div (BinOp Sub (BinOp Mul (diffExpr v f) g) (BinOp Mul (diffExpr v g) f)) (BinOp Mul g g)
 BinOp Exp f g -> --Chain Rule
  BinOp Mul (BinOp Mul g (BinOp Exp f (BinOp Sub g (Num 1)))) (diffExpr v f)
 Neg n -> Neg (diffExpr v n)
 Var v'  --Treated like partial differentiation w/ respect to v. Other vars treated as constants
  | v' == v -> Num 1.0
  | otherwise -> Num 0.0
 Num _ -> Num 0




newtM :: String -> Double -> Expr -> Double
newtM v guess f =
  let f' = diffExpr v f
      nextGuess = guess - (evalAt v guess f) / (evalAt v guess f')
  in if abs (nextGuess - guess) <= epsilon then nextGuess
     else newtM v nextGuess f