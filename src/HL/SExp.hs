module HL.SExp where

import Text.Parsec
import Control.Monad (void, foldM)
import Data.Either.Combinators (mapLeft)
import Data.Char (isLower, isAlphaNum, isSpace)
import qualified Data.Set as Set
import Text.Read (readMaybe)

import HL.AST
import Pretty


data SExp = Leaf String | Node [SExp] deriving (Show, Eq)

instance Pretty SExp where
  pretty (Leaf s) = s
  pretty (Node es) = "(" ++ (unwords . map pretty) es ++ ")"

-- External Interface --

fromFile :: String -> String -> Either String [SExp]
fromFile name input = mapLeft show $ parse fileParser name input


parseProgram :: String -> String -> Either String Program
parseProgram name input = fromFile name input >>= toProgram


parseModule :: String -> String -> Either String [Definition]
parseModule name input = fromFile name input >>= toModule

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
matching _ = undefined


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


-- Transformer --

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

toProgram :: [SExp] -> Either String Program
toProgram [] = Left "No expression"
toProgram xs = Program <$> defs <*> expr
  where
    defs = mapM toDef . init $ xs
    expr = toExpr . last $ xs


toModule :: [SExp] -> Either String [Definition]
toModule = mapM toDef


toDef :: SExp -> Either String Definition
toDef (Node [Leaf "define", Node (Leaf name:args), value]) = Def name <$> lam
  where
    argsNames = mapM toArg args
    lam = Lambda <$> argsNames <*> toExpr value
toDef (Node [Leaf "define", name, value]) = Def <$> toIdent name <*> toExpr value
toDef bad = Left $ message "definition" bad


toExpr :: SExp -> Either String Exp
toExpr expr@(Leaf _) = either (const (Var <$> toIdent expr)) Right $ toNat expr
toExpr lam@(Node (Leaf "lambda" : _)) = toLambda lam
toExpr lam@(Node (Leaf "λ" : _)) = toLambda lam
toExpr lt@(Node (Leaf "let" : _)) = toLet lt
toExpr letrec@(Node (Leaf "letrec" : _)) = toLetrec letrec
toExpr (Node (f:xs)) = toExpr f >>= (\f' -> foldM doApp f' xs)
  where
    doApp f' arg = Application f' <$> toExpr arg
toExpr (Node []) = Left "Illegal empty expression"


toLambda :: SExp -> Either String Exp
toLambda (Node [Leaf _, Node args, body]) = Lambda <$> argsM <*> bodyM
  where
    argsM = mapM toArg args
    bodyM = toExpr body
toLambda bad = Left $ message "lambda" bad


toLet :: SExp -> Either String Exp
toLet (Node [Leaf "let", Node bindings, body]) = Let <$> bindingsM <*> bodyM
  where
    bindingsM = mapM toBinding bindings
    toBinding (Node [Leaf name, value]) = (\e -> (name,e)) <$> toExpr value
    toBinding bad = Left $ message "binding" bad
    bodyM = toExpr body
toLet bad = Left $ message "let" bad


toLetrec :: SExp -> Either String Exp
toLetrec (Node [Leaf "letrec", Node [Leaf name, expr], body]) = Letrec name <$> toExpr expr <*> toExpr body
toLetrec bad = Left $ message "letrec" bad


toNat :: SExp -> Either String Exp
toNat l@(Leaf n) = maybe (Left (message "number" l)) (Right . Num) $ readMaybe n
toNat bad = Left $ message "number" bad


toArg :: SExp -> Either String String
toArg (Leaf "_") = Right "_"
toArg arg = mapLeft (const (message "argument" arg)) $ toIdent arg


toIdent :: SExp -> Either String String
toIdent sexp@(Leaf name@(first:rest)) = if valid
                                        then Right name
                                        else Left $ message "identifier" sexp
  where
    valid = checkFirst first && all checkRest rest
    checkFirst c = isLower c || Set.member c symbols
    checkRest c = isAlphaNum c || Set.member c symbols || c == '_'
    symbols = Set.fromList "!@#$%^&*+-=<>?/~"
toIdent bad = Left $ message "identifier" bad


message :: String -> SExp -> String
message s e = "Invalid " ++ s ++ " " ++ pretty e
