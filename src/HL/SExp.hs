module HL.SExp where

import Text.Parsec
import Control.Monad (void)
import Data.Char (isSpace)

import Pretty


data SExp = Leaf String | Node [SExp] deriving (Show, Eq)

instance Pretty SExp where
  pretty (Leaf s) = s
  pretty (Node es) = "(" ++ (unwords . map pretty) es ++ ")"

-- Parser --

type Parser = Parsec String ()


fileParser :: Parser [SExp]
fileParser = do
  optional sep
  expressions <- sepEndBy sExp (optional sep)
  eof
  return expressions


sExp :: Parser SExp
sExp = node <|> literal
  where
    literal = Leaf <$> many1 (satisfy (\c -> not (isSpace c || elem c ")]};")))
    node = do
      open <- oneOf "([{"
      optional sep
      inside <- sepEndBy sExp sep
      _ <- matching open
      return $ Node inside


matching :: Char -> Parser Char
matching '(' = char ')'
matching '{' = char '}'
matching '[' = char ']'
matching bad = unexpected $ "cannot match " ++ show bad


sep :: Parser ()
sep = skipMany1 (whitespace <|> lineComment <|> blockComment <?> "whitespace")
  where
    whitespace = skipMany1 $ satisfy isSpace
    lineComment = try (char ';') >> skipMany (noneOf "\n\r")


blockComment :: Parser ()
blockComment = void $ try (string "#|") >> inComment
  where
    inComment =   try (string "|#")
              <|> (blockComment >> inComment)
              <|> (skipMany1 (noneOf "#|") >> inComment)
              <|> (oneOf "#|" >> inComment)
