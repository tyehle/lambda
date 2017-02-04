module HL.Parser (parseHL) where

import HL.AST
import Text.Parsec
import Control.Monad (void)
import qualified Data.Bifunctor as Bifunctor (first)

parseHL :: String -> Either String Program
parseHL = Bifunctor.first show . parse programP "input"

type Parser a = Parsec String () a

-- <prg> ::= <def> ... <exp>
--
-- <def> ::= (define <var> <exp>)
--        |  (define (<var> <var> ...) <exp>)
--
-- <exp> ::= <var>
--
--        |  #t
--        |  #f
--        |  (if  <exp> <exp> <exp>)
--        |  (and <exp> <exp>)
--        |  (or  <exp> <exp>)
--        |  (not <exp>)
--
--        |  <nat>
--        |  (zero? <exp>)
--        |  (- <exp> <exp>)
--        |  (= <exp> <exp>)
--        |  (+ <exp> <exp>)
--        |  (* <exp> <exp>)
--        |  (/ <exp> <exp>)
--        |  (even? <exp>)
--
--        |  (λ (<var> ...) <exp>)
--        |  (let ((<var> <exp>) ...) <exp>)
--        |  (letrec (<var> <exp>) <exp>)
--
--        |  (cons <exp> <exp>)
--        |  (head <exp>)
--        |  (tail <exp>)
--        |  (pair? <exp>)
--        |  (null? <exp>)
--        |  '()
--
--        |  (<exp> <exp> ...)

programP :: Parser Program
-- programP = expressionP <* eof
programP = Program <$> sepEndBy (try definitionP) whitespace <*> expressionP <* spaces <* eof

definitionP :: Parser Definition
definitionP = inParens $ do
  _ <- word "define"
  whitespace
  defVal <|> defFun
  where
    defVal = do
      name <- identifierP
      whitespace
      body <- expressionP
      return $ Def name body
    defFun = do
      (name, args) <- inParens ((,) <$> (identifierP <* whitespace) <*> sepEndBy identifierP whitespace)
      whitespace
      body <- expressionP
      return . Def name $ Lambda args body


expressionP :: Parser Exp
expressionP =  (word "#t" *> return VTrue)
           <|> (word "#f" *> return VFalse)
           <|> try ifP
           <|> try (fn2 "and" And)
           <|> try (fn2 "or" Or)
           <|> try (fn1 "not" Not)

           <|> Num . read <$> many1 digit
           <|> try (fn1 "zero?" IsZero)
           <|> try (fn2 "-" Minus)
           <|> try (fn2 "+" Plus)
           <|> try (fn2 "*" Mult)
           <|> try (fn2 "/" Divide)
           <|> try (fn2 "=" Eq)
           <|> try (fn1 "even?" IsEven)

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

           <|> Var <$> try identifierP

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
  (name, binding) <- inParens ((,) <$> (identifierP <* whitespace) <*> expressionP)
  whitespace
  body <- expressionP
  return $ Letrec name binding body

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

-- must not include whitespace or ')'
identifierP :: Parser String
identifierP = (:) <$> (lower <|> symbols) <*> many (alphaNum <|> symbols)
  where
    symbols = oneOf "!@#$%^&*_+-=|:;<>?,./~"

whitespace :: Parser ()
whitespace = void $ many1 space

word :: String -> Parser String
word = try . string

inParens :: Parser a -> Parser a
inParens p = char '(' *> spaces *> p <* spaces <* char ')'
