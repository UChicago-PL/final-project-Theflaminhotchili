module Main where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Data.List (intercalate, maximumBy, nub)
import Data.Ord (comparing)
import Control.Exception
import System.IO
import System.Exit (exitSuccess)

data Expr
  = Num Double
  | Var String
  | Neg Expr
  | BinOp Op Expr Expr
  deriving (Show, Eq)

data Eqn = Eqn Expr Expr
  deriving (Show, Eq)

data SysEq = SysEq Eqn Eqn
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
      terms = (filter (/= "") (map (showTerm "x") xt)) ++ (filter (/= "") (map (showTerm "y") yt))
    in intercalate "+" terms

getCTerm :: Poly -> Double
getCTerm p = Data.Maybe.fromMaybe 0.0 (lookup 0 (xTerms p))


-- ===================================== --
-- ======== MAIN LOOP FUNCTIONS ======== --
-- ===================================== --


main :: IO ()
main = do
    putStr "> "
    input <- getLine
    if input == "quit" || input == "exit"
        then exitSuccess
        else do
            result <- try (handleInput input) :: IO (Either SomeException ())
            case result of
                Left err -> putStrLn ("Error: " ++ show err)
                Right _  -> return ()
            main
    main


parseInput :: String -> Either Expr (Either Eqn SysEq)
parseInput input
  | ';' `elem` input = case filter (null . snd) (readP_to_S sysEq (filter (/= ' ') input)) of
                       [(result,_)] -> Right (Right result)
                       _ -> error ("BadArgument: Could not find a valid system of equations in \""++input++"\"")
  | '=' `elem` input = case filter (null . snd) (readP_to_S eqn (filter (/= ' ') input)) of
                       [(result,_)] -> Right (Left result)
                       _ -> error ("BadArgument: Could not find a valid equation in \""++input++"\"")
  | otherwise = case filter (null . snd) (readP_to_S expr (filter (/= ' ') input)) of
                [(result,_)] -> Left result
                _ -> error ("BadArgument: Could not find a valid expression in \""++input++"\"")


handleInput :: String -> IO ()
handleInput input = case parseInput input of
    Left e          -> handleExpr e
    Right (Left q)   -> handleEqn q
    Right (Right sys)  -> handleSys sys

handleExpr :: Expr -> IO ()
handleExpr e = case getVars e of
    []  -> putStrLn ("= " ++ show (evalExpr e))
    [v] -> putStrLn ("Roots: " ++ show (findExprRoots v e))
    _   -> putStrLn "Error: too many variables in expression"

handleEqn :: Eqn -> IO ()
handleEqn q@(Eqn l r) = case getVars (BinOp Sub l r) of
    []       -> let lVal = evalExpr l
                    rVal = evalExpr r
                in putStrLn (show lVal ++ " = " ++ show rVal ++ " -> " ++ show (lVal == rVal))
    [v]      -> putStrLn ("Roots: " ++ show (findExprRoots v (toExpr q)))
    [v1, v2] -> let (x:y:rest) = classifyVars [v1, v2]
                in putStrLn ("Solutions: " ++ show (findExprRoots x (isolateVar x y q)))
    _        -> putStrLn "Error: too many variables"

handleSys :: SysEq -> IO ()
handleSys sys@(SysEq l r) =
    let vars = nub (getVars (toExpr l) ++ getVars (toExpr r))
    in case vars of
        [v1, v2] -> let (x:y:rest) = classifyVars [v1, v2]
                    in putStrLn ("Solutions: " ++ show (solveSysEq x y sys))
        _        -> putStrLn "Error: system requires exactly 2 variables"


getVars :: Expr -> [String] --find variables in a
getVars = nub . findVars
  where
    findVars (Var v)       = [v]
    findVars (Num _)       = []
    findVars (Neg e)       = findVars e
    findVars (BinOp _ f g) = findVars f ++ findVars g


classifyVars :: [String] -> [String]
classifyVars vars = filter (`elem` vars) ["x", "y"]


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

sysEq :: ReadP SysEq
sysEq = do
    lhs <- eqn
    skipSpaces
    _ <- char ';'
    skipSpaces
    SysEq lhs <$> eqn
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

newtM :: String -> Double -> Expr -> Int -> Maybe Double
newtM v guess f 0 = Nothing
newtM v guess f n =
    let f' = diffExpr v f
        f'val = evalAt v guess f'
        nextGuess = guess - (evalAt v guess f) / f'val
    in if abs f'val <= epsilon then Nothing
       else if abs (nextGuess - guess) <= epsilon then Just nextGuess
       else newtM v nextGuess f (n-1)

findExprRoots :: String -> Expr -> [Double]
findExprRoots v f =
    let p       = toPoly v "$" f
        guesses = getSamplePoints p
        results = mapMaybe (\g -> newtM v g f 1000) guesses
        rounded = map (\x -> if abs (x - fromIntegral (round x)) < 1e-6
                             then fromIntegral (round x)
                             else x) results
    in nub (cleanRoots rounded)

cleanRoots :: [Double] -> [Double]
cleanRoots [] = []
cleanRoots (x:y:rest)
  | abs(x-y)<1e-3 = (cleanRoots (x:rest))
  | otherwise = x:(cleanRoots (y:rest))
cleanRoots [x] = [x]

-- ======================================= --
-- ======== EXPRESSION CONVERSION ======== --
-- ======================================= --

toExpr :: Eqn -> Expr
toExpr (Eqn l r) = BinOp Sub l r


termsToExpr :: String -> [(Deg,Coef)] -> Expr
termsToExpr x [] = (Num 0.0)
termsToExpr x ((d,c):[])
  | d==0 = Num c
  | d > 0 = BinOp Mul (BinOp Exp (Var x) (Num (fromIntegral d))) (Num c)
  | otherwise = Num 0.0
termsToExpr x ((d,c):rest)
  | d==0 = BinOp Add (Num c) (termsToExpr x rest)
  | d > 0 = BinOp Add (BinOp Mul (BinOp Exp (Var x) (Num (fromIntegral d))) (Num c)) (termsToExpr x rest)
  | otherwise = BinOp Add (Num 0) (termsToExpr x rest)

















-- ======================================== --
-- ======== POLYNOMIAL AGGREGATION ======== --
-- ======================================== --

toPoly :: String -> String -> Expr -> Poly
toPoly x y e = case e of
  Var v'
    | v' == x -> Poly [(1,1),(0,0)] []
    | v' == y -> Poly [(0,0)] [(1,1)]
    | otherwise -> error "Extra variable" --FIX YTERMS
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
  | not (null (yTerms p1)) = Poly (mulPolyTerms (xTerms p1) (xTerms p2))
                                (applyToSnd (* getCTerm p2) (yTerms p1))
  | not (null (yTerms p2)) = Poly (mulPolyTerms (xTerms p1) (xTerms p2))
                                (applyToSnd (* getCTerm p1) (yTerms p2))
  | otherwise = Poly (mulPolyTerms (xTerms p1) (xTerms p2)) []

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





-- ============================================= --
-- ======== SYSTEM OF EQUATIONS SOLVING ======== --
-- ============================================= --

isolateVar :: String -> String -> Eqn -> Expr --Solves for y in terms of x
isolateVar x y q =
  let e = toExpr q
      p = toPoly x y e
      ex = termsToExpr x (xTerms p)
      (dy,cy) = maximumBy (comparing fst) (yTerms p)
  in if (dy /= 1) then error "Equation is polynomial in y. Only expressions and equations linear in y are supported"
     else BinOp Div ex (Neg (Num cy))


substVar :: String -> Expr -> Expr -> Expr
substVar v subst e = case e of
  Var v'
    | v' == v -> subst
    | otherwise -> e
  Num n -> e
  Neg n -> Neg (substVar v subst n)
  BinOp Add f g -> BinOp Add (substVar v subst f) (substVar v subst g)
  BinOp Sub f g -> BinOp Sub (substVar v subst f) (substVar v subst g)
  BinOp Mul f g -> BinOp Mul (substVar v subst f) (substVar v subst g)
  BinOp Div f g -> BinOp Div (substVar v subst f) (substVar v subst g)
  BinOp Exp f g -> BinOp Exp (substVar v subst f) (substVar v subst g)

solveSysEq :: String -> String -> SysEq -> [(Double, Double)]
solveSysEq x y (SysEq l r) =
  let yExpr = isolateVar x y l
      rExpr = toExpr r
      subRExpr = substVar y yExpr rExpr
      xRoots = findExprRoots x subRExpr
  in nub (map (\xVal -> (xVal, evalAt x xVal yExpr)) xRoots)


