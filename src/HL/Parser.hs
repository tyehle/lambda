module HL.Parser (parseProgram, parseModule) where

import HL.AST
import Text.Parsec
import Control.Monad (void)
import Data.Bifunctor (first)

parseProgram :: String -> String -> Either String Program
parseProgram = lexParse programP

parseModule :: String -> String -> Either String [Definition]
parseModule = lexParse moduleP

type Parser a = Parsec String () a

lexParse :: Parser a -> String -> String -> Either String a
lexParse p name input = first show $ parse commentP name input >>= parse p name

commentP :: Parser String
commentP = concat <$> sepEndBy (many (noneOf ";"))
                               (char ';' *> many (noneOf "\n\r"))

-- <prg> ::= <def> ... <exp>
--
-- <mod> ::= <def> ...
--
-- <def> ::= (define <var> <exp>)
--        |  (define (<var> <arg> ...) <exp>)
--
-- <exp> ::= <var>
--
--        |  <nat>
--
--        |  (<lam> (<arg> ...) <exp>)
--        |  (let ((<var> <exp>) ...) <exp>)
--        |  (letrec (<var> <exp>) <exp>)
--
--        |  (<exp> <exp> ...)
--
-- <arg> ::= _ | <var>
--
-- <lam> ::= λ | lambda

moduleP :: Parser [Definition]
moduleP = spaces *> sepEndBy definitionP whitespace <* eof

programP :: Parser Program
programP = Program <$> (spaces *> sepEndBy (try definitionP) whitespace) <*> expressionP <* spaces <* eof

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
      (name, args) <- inParens ((,) <$> (identifierP <* whitespace) <*> sepEndBy argP whitespace)
      whitespace
      body <- expressionP
      return . Def name $ Lambda args body


expressionP :: Parser Exp
expressionP =  try lamP
           <|> try letP
           <|> try letrecP
           <|> Num . read <$> many1 digit
           <|> try appP
           <|> Var <$> try identifierP


lamP :: Parser Exp
lamP = inParens $ do
  _ <- word "lambda" <|> word "λ"
  whitespace
  args <- inParens (sepEndBy1 argP whitespace)
  whitespace
  body <- expressionP
  return $ Lambda args body

letP :: Parser Exp
letP = inParens $ do
  _ <- word "let"
  whitespace
  bindings <- inParens (sepEndBy (inParens ((,) <$> (identifierP <* whitespace) <*> expressionP)) whitespace)
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



-- must not include whitespace or ")}]"
identifierP :: Parser String
identifierP = (:) <$> (lower <|> symbols) <*> many (alphaNum <|> symbols <|> char '_')
  where
    symbols = oneOf "!@#$%^&*+-=<>?/~"

argP :: Parser String
argP = word "_" <|> identifierP

whitespace :: Parser ()
whitespace = void $ many1 space

word :: String -> Parser String
word = try . string

inParens :: Parser a -> Parser a
inParens p  =  char '(' *> inner <* char ')'
           <|> char '[' *> inner <* char ']'
           <|> char '{' *> inner <* char '}'
  where
    inner = spaces *> p <* spaces
