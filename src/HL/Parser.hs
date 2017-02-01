module HL.Parser (parseHL) where

import HL.AST
import Text.Parsec
import Control.Monad (void)
import Data.Bifunctor (first)

parseHL :: String -> Either String Exp
parseHL = first show . parse programP "input"

type Parser a = Parsec String () a

programP :: Parser Exp
programP = expressionP <* eof

expressionP :: Parser Exp
expressionP =  Var <$> try identifierP

           <|> (word "#t" *> return VTrue)
           <|> (word "#f" *> return VFalse)
           <|> try ifP
           <|> try (fn2 "and" And)
           <|> try (fn2 "or" Or)

           <|> Num . read <$> many1 digit
           <|> try (fn1 "zero?" IsZero)
           <|> try (fn2 "-" Minus)
           <|> try (fn2 "+" Plus)
           <|> try (fn2 "*" Mult)
           <|> try (fn2 "/" Divide)
           <|> try (fn2 "=" Eq)

           <|> try lamP
           <|> try letP
           <|> try letrecP

           <|> try (fn2 "cons" Cons)
           <|> try (fn1 "head" Head)
           <|> try (fn1 "tail" Tail)
           <|> try (fn1 "pair?" IsPair)
           <|> try (fn1 "null?" IsNull)
           <|> try (word "()" *> return VEmpty)
           <|> try appP

ifP :: Parser Exp
ifP = inParens $ do
  _ <- word "if" <* many1 space
  c <- expressionP <* many1 space
  t <- expressionP <* many1 space
  f <- expressionP
  return $ If c t f

lamP :: Parser Exp
lamP = inParens $ do
  _ <- word "lambda" <|> word "λ"
  whitespace
  args <- inParens (sepEndBy1 identifierP whitespace)
  whitespace
  body <- expressionP
  return $ Lambda args body

letP :: Parser Exp
letP = inParens $ do
  _ <- word "let"
  whitespace
  bindings <- inParens (sepEndBy1 (inParens ((,) <$> (identifierP <* whitespace) <*> expressionP)) whitespace)
  whitespace
  Let bindings <$> expressionP

letrecP :: Parser Exp
letrecP = inParens $ do
  _ <- word "letrec"
  whitespace
  (name, Lambda args fn) <- inParens ((,) <$> (identifierP <* whitespace) <*> lamP)
  whitespace
  body <- expressionP
  return $ Letrec (name, (args, fn)) body

appP :: Parser Exp
appP = inParens $ do
  f <- expressionP
  whitespace
  args <- sepEndBy1 expressionP whitespace
  return $ foldl Application f args


fn1 :: String -> (Exp -> Exp) -> Parser Exp
fn1 s builder = inParens $ builder <$> (word s *> many1 space *> expressionP)

fn2 :: String -> (Exp -> Exp -> Exp) -> Parser Exp
fn2 s builder = inParens $ do
  _ <- word s
  whitespace
  r <- expressionP
  whitespace
  l <- expressionP
  return $ builder r l

identifierP :: Parser String
identifierP = (:) <$> lower <*> many alphaNum

whitespace :: Parser ()
whitespace = void $ many1 space

word :: String -> Parser String
word = try . string

inParens :: Parser a -> Parser a
inParens p = char '(' *> spaces *> p <* spaces <* char ')'
