module Lambda where

import Data.List (nub, (\\))
import Data.Char (chr, ord)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (Abs x e) = x : (vars e \\ [x])
vars (App e1 e2) = vars e1 ++ (vars e2 \\ vars e1)
vars (Macro x) = []


-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Macro x) = []
 
-- 1.3.
newVar :: [String] -> String
newVar vars = let oneChar = map (:[]) ['a'..'z']
                  twoChars = oneChar ++ [a ++ b | a <- oneChar, b <- oneChar]
                  threeChars = twoChars ++ [a ++ b | a <- twoChars, b <- oneChar]
              in head (filter (\s -> not (elem s vars)) threeChars )

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var x) = True
isNormalForm (Abs x e) = isNormalForm e
isNormalForm (App e1 e2) = case e1 of
  Abs x e3 -> False
  _ -> isNormalForm e1 && isNormalForm e2


-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce y e1 e2 = case e1 of
  Var x -> if x == y then e2 else e1
  Abs x e -> if x == y then e1 
  else if not (x `elem` freeVars e2) then Abs x (reduce y e e2)
  else let new = newVar (vars e1)
    in reduce y (Abs new $ reduce x e (Var new)) e2
  App e3 e4 -> App (reduce y e3 e2) (reduce y e4 e2)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (Var x) = Var x
normalStep (App e1 e2) = case e1 of
  Abs x e -> reduce x e e2
  _ -> if isNormalForm e1 then App e1 $ normalStep e2 else App (normalStep e1) e2
normalStep (Abs x e) = Abs x $ normalStep e
 
-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (Var x) = Var x
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep (App e1 e2) 
  | not (isNormalForm e1) = App (applicativeStep e1) e2
  | not (isNormalForm e2) = App e1 (applicativeStep e2)
  | otherwise = case e1 of
      Abs x e -> reduce x e e2
      _ -> App e1 e2

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step expr = if isNormalForm expr then [expr] else expr : (simplify step $ step expr)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep




