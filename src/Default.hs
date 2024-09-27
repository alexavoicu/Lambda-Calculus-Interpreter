module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue = Abs "x" (Abs "y" $ Var "x")
bFalse = Abs "x" (Abs "y" $ Var "y")
bAnd = Abs "a" (Abs "b" (App (App (Var "a") (Var "b")) (Var "a")))
bOr = Abs "a" (Abs "b" (App (App (Var "a") (Var "a")) (Var "b")))
bNot = Abs "a" (App (App (Var "a") bFalse) bTrue)
bXor = Abs "a" (Abs "b" (App (App (Var "a") (App bNot (Var "b"))) $ Var "b"))

-- 4.2. Pair encodings
pair = Abs "a" (Abs "b" (Abs "z" (App (App (Var "z") (Var "a")) (Var "b"))))
first = Abs "p" (App (Var "p") bTrue)
second = Abs "p" (App (Var "p") bFalse)

-- 4.3. Natural number encodings
n0 = Abs "f" (Abs "y" (Var "y"))
n1 = Abs "f" (Abs "y" (App (Var "f") (Var "y")))
n2 = Abs "f" (Abs "y" (App (Var "f") (App (Var "f") (Var "y"))))
nSucc = Abs "n" (Abs "f" (Abs "y" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "y")))))
nPred = Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "u" (Var "u")))))
nAdd = Abs "n" (Abs "m" (Abs "f" (Abs "y" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "y"))))))
nSub = Abs "n" (Abs "m" (App (App (Var "m") nPred) (Var "n")))
nMult = App fix multAux

fix = Abs "f" (App (Abs "x" (App (Var "f") (App (Var "x") (Var "x")))) (Abs "x" (App (Var "f") (App (Var "x") (Var "x")))))
multAux = Abs "r" (Abs "n" (Abs "m" (Abs "f" (Abs "y" (App (App (Var "n") (App (Var "m") (Var "f"))) (Var "y"))))))


-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
