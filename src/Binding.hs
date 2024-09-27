module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l



introduceMacros :: Context -> Lambda -> Maybe Lambda
introduceMacros c expr = case expr of
    Var x -> Just(Var x)
    Abs x e -> do 
        e1 <- introduceMacros c e
        return (Abs x e1)
    App e1 e2 -> do
        a <- introduceMacros c e1
        b <- introduceMacros c e2
        return (App a b)
    Macro x -> lookup x c



-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx c step expr = case introduceMacros c expr of
    Nothing -> Left "error"
    Just (e) -> Right(simplify step e)


normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
