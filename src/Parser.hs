module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
instance Monad Parser where
    return v = Parser $ \s -> Just(v, s)
    mp >>= f = Parser $ \s -> case parse mp s of 
                                Nothing -> Nothing
                                Just (v, rest) -> parse (f v) rest
instance Applicative Parser where
  af <*> mp = 
    do 
      f <- af
      v <- mp
      return $ f v
  pure = return

instance Functor Parser where 
  fmap f mp = 
    do 
      x <- mp
      return $ f x

failParser :: Parser a 
failParser = Parser $ \s -> Nothing

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of 
                                Nothing -> parse p2 s
                                x -> x

plusParser :: Parser a -> Parser [a]
plusParser p = do
                x <- p
                xs <- starParser p
                return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = (plusParser p) <|> (return [])

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> 
                       case s of 
                        [] -> Nothing
                        (x:xs) -> if p x then Just (x, xs) else Nothing 

varParser :: Parser String
varParser = do 
            x <- predicateParser isAlpha
            xs <- starParser (predicateParser isAlphaNum)
            return (x:xs)

charParser :: Char -> Parser Char
charParser c = Parser $ \s -> 
                case s of 
                    [] -> Nothing
                    (x:xs) -> if x == c then Just(x,xs) else Nothing 

whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

varLambdaParser :: Parser Lambda
varLambdaParser = Var <$> varParser

absLambdaParser :: Parser Lambda
absLambdaParser = 
    do
        charParser '\\'
        x <- varParser
        charParser '.'
        e <- exprParser
        return (Abs x e)

macroParser :: Parser String
macroParser = do 
            x <- predicateParser (\c -> isAlphaNum c && (isUpper c || isDigit c))
            xs <- starParser (predicateParser isAlphaNum)
            return (x:xs)

macroLambdaParser :: Parser Lambda
macroLambdaParser = Macro <$> macroParser
    
appLambdaParser :: Parser Lambda
appLambdaParser = 
    do 
        charParser '('
        e1 <- exprParser
        charParser ' '
        e2 <- exprParser
        charParser ')'
        return (App e1 e2)

exprParser :: Parser Lambda 
exprParser = absLambdaParser <|> appLambdaParser <|> macroLambdaParser <|> varLambdaParser

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda s = case parse exprParser s of
    Just(expr, "") -> expr
    _ -> Var "error"


lineMacroParser :: Parser Line
lineMacroParser = 
    do
        name <- macroParser
        charParser '='
        e <- exprParser
        return (Binding name e)
        

-- 3.3.
parseLine :: String -> Either String Line
parseLine line = case parse lineMacroParser line of
    Nothing -> case parseLambda line of
        Var x -> if x == "error" then Left "error" else Right (Eval (Var x))
        e -> Right (Eval e)
    Just (name, l) -> Right (name)


