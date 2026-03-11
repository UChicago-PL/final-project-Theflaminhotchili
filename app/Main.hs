module Main where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Data.List (intercalate, maximumBy, nub)
import Data.Ord (comparing)

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

type Coef = Double
type Deg = Int

data Poly = Poly
  {xTerms    :: [(Deg, Coef)]
  ,yTerms    :: [(Deg, Coef)]
  }
  deriving (Eq)


showTerm :: String -> (Deg, Coef) -> String
showTerm _ (_,0) = show ""
showTerm _ (0, c) = show c
showTerm var (1, c) = show c ++ var
showTerm var (d, c) = show c ++ var ++ "^" ++ show d

instance Show Poly where
  show (Poly xt yt) =
    let
      terms = (map (showTerm "x") xt) ++ (map (showTerm "y") yt)
    in intercalate "+" terms

getCTerm :: Poly -> Double
getCTerm p = Data.Maybe.fromMaybe 0.0 (lookup 0 (xTerms p))

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

evalPoly :: Double -> Poly -> Double
evalPoly x p =
  let terms = xTerms p
  in sum (map (\(d,c) -> x**(fromIntegral d) * c) terms)


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



-- ======================================= --
-- ======== POLYNOMIAL EVALUATION ======== --
-- ======================================= --

--Use Cauchy's Bound to find a search area for the roots of a polynomial
getCauchyBound :: Poly -> Double
getCauchyBound p =
  let terms = xTerms p
      leading = maximumBy (comparing fst) terms
      rest = filter (/= leading) terms
      maxCoef = maximumBy (comparing (abs . snd)) rest
  in 1 + (abs (snd maxCoef)) / abs (snd leading)

getSamplePoints :: Poly -> [Double]
getSamplePoints p =
  let maxDeg = fst (maximumBy (comparing fst) (xTerms p))
      r = getCauchyBound p
      stepSize = (2*r) / (100*fromIntegral maxDeg)
  in [(-r),(-r) + stepSize .. r]

-- getSignChanges :: [(Double, Double)] -> [Double]
-- getSignChanges [] = []
-- getSignChanges [_] = [0.0]
-- getSignChanges ((x1,v1):(x2,v2):rest)
--   | v1*v2 < 0 = ((x1+x2)/2) : getSignChanges ((x2,v2):rest)
--   | otherwise = getSignChanges ((x2,v2):rest)

-- getCritPoints :: Poly -> [Double]
-- getCritPoints p =
--   let dp = diffPoly p
--       critGuesses = findSignChanges dp
--   in filter (\x -> abs (evalPoly x p) < epsilon) critGuesses


-- findSignChanges :: Poly -> [Double]
-- findSignChanges p =
--   let points = getSamplePoints p
--       vals = map (\x -> (x,evalPoly x p)) points
--   in getSignChanges vals

-- findGuesses :: Poly -> [Double]
-- findGuesses p =
--   let sc = findSignChanges p
--       cp = getCritPoints p
--   in sc ++ cp

newtM :: String -> Double -> Expr -> Double
newtM v guess f =
  let f' = diffExpr v f
      nextGuess = guess - (evalAt v guess f) / (evalAt v guess f')
  in if abs (nextGuess - guess) <= epsilon then nextGuess
     else newtM v nextGuess f

findExprRoots :: String -> Expr -> [Double]
findExprRoots v f =
  let p = toPoly v "$" f
      guesses = getSamplePoints p
  in nub (map (\x -> if abs (x-fromIntegral (round x)) < 1e-6 then fromIntegral (round x) else x) (cleanRoots (map (\g -> (newtM v g f)) guesses)))


cleanRoots :: [Double] -> [Double]
cleanRoots [] = []
cleanRoots (x:y:rest)
  | abs(x-y)<1 = (cleanRoots (x:rest))
  | otherwise = x:(cleanRoots (y:rest))
cleanRoots [x] = [x]




-- ======================================== --
-- ======== POLYNOMIAL AGGREGATION ======== --
-- ======================================== --

toPoly :: String -> String -> Expr -> Poly
toPoly x y e = case e of
  Var v'
    | v' == x -> Poly [(1,1),(0,0)] []
    | v' == y -> Poly [(0,0)] [(1,1)]
    | otherwise -> error "Extra variable"
  Num n -> Poly [(0,n)] []
  Neg n -> negPoly (toPoly x y n)
  BinOp Add f g -> addPoly (toPoly x y f) (toPoly x y g)
  BinOp Sub f g -> addPoly (toPoly x y f) (negPoly (toPoly x y g))
  BinOp Mul f g -> mulPoly (toPoly x y f) (toPoly x y g)
  BinOp Div f g -> divPoly (toPoly x y f) (toPoly x y g)
  BinOp Exp f g -> expPoly (toPoly x y f) (toPoly x y g)

diffPoly :: Poly -> Poly
diffPoly p =
  Poly (filter ((>= 0).fst) (map (\(d,c) -> (d-1, fromIntegral d*c)) (xTerms p))) []



sumByFst :: (Ord k, Num v) => [(k, v)] -> [(k, v)]
sumByFst tuples = Map.toList (Map.fromListWith (+) tuples)

applyToSnd :: (b-> c) -> [(a,b)] -> [(a,c)]
applyToSnd f = map (\(x,y) -> (x, f y))

addPoly:: Poly -> Poly -> Poly --Add 2 polynomials
addPoly p1 p2 =
  let sumX = sumByFst (xTerms p1 ++ xTerms p2)
      sumY = sumByFst (yTerms p1 ++ yTerms p2)
  in Poly sumX sumY

negPoly:: Poly -> Poly --Negate Polynomial
negPoly p =
  let negX = applyToSnd negate (xTerms p)
      negY = applyToSnd negate (yTerms p)
  in Poly negX negY

mulPolyTerms :: [(Deg, Coef)] -> [(Deg, Coef)] -> [(Deg, Coef)]
mulPolyTerms x1s x2s = sumByFst [ (d1+d2, c1*c2) | (d1,c1) <- x1s, (d2,c2) <- x2s ]


mulPoly :: Poly -> Poly -> Poly
mulPoly p1 p2
  | length (xTerms p1) > 1 && not (null (yTerms p2)) = error "Cannot multiply 2 different variables"
  | length (xTerms p2) > 1 && not (null (yTerms p1)) = error "Cannot multiply 2 different variables"
  | not (null (yTerms p1)) && not (null (yTerms p2)) = error "Nonlinear in dependent variable"
  | null (yTerms p1) = Poly (mulPolyTerms (xTerms p1) (xTerms p2)) []
  | not (null (yTerms p1)) = Poly (mulPolyTerms (xTerms p1) (xTerms p2))
                                (applyToSnd (* getCTerm p1) (yTerms p2))
  | otherwise = Poly (mulPolyTerms (xTerms p1) (xTerms p2))
                                (applyToSnd (* getCTerm p2) (yTerms p1))

divPoly :: Poly -> Poly -> Poly
divPoly p1 p2
  | length (xTerms p2) > 1 || not (null (yTerms p2)) =
    error "Rational Expressions (division by a polynomial) are not supported"
  | getCTerm p2 == 0.0 = error "Division by 0 error"
  | otherwise = let cTerm = getCTerm p2
                in Poly (applyToSnd (/ cTerm) (xTerms p1)) (applyToSnd (/ cTerm) (yTerms p1))

expRec :: Poly -> Int -> Poly
expRec p 0 = Poly [(0,1.0)] []
expRec p 1 = p
expRec p n = mulPoly p (expRec p (n-1))

expPoly :: Poly -> Poly -> Poly
expPoly p p2
  | length (xTerms p2) > 1 || not (null (yTerms p2)) =
    error "Variables in exponents are not supported"
  | (length (xTerms p) > 1) && null (yTerms p) =
    let e = round (getCTerm p2)
    in expRec p e
  | otherwise = error "error in exponentiation"