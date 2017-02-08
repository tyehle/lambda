module Parser where

import Node

import Text.Parsec

parseNode :: String -> Node
parseNode = either (error . show) id . parse program "input"

program :: Parsec String () Node
program = factor <* eof

factor :: Parsec String () Node
factor = spaces *> (foldl1 App <$> sepEndBy1 term spaces)

term :: Parsec String () Node
term = parens <|> lambda <|> reference
  where
    parens = char '(' *> factor <* char ')'
    reference = Ref <$> identifier

lambda :: Parsec String () Node
lambda = do
  _ <- char '\\'
  spaces
  args <- sepEndBy1 identifier spaces
  _ <- char '.'
  body <- factor
  return $ foldr Lam body args

identifier :: Parsec String () String
identifier = (:) <$> lower <*> many alphaNum
