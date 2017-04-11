module HL.Typed where

import HL.SExp
import Pretty

import qualified Data.Set as Set
import Text.Parsec (parse)
import Control.Monad (foldM)
import Data.Either.Combinators (mapLeft)
import Data.Char (isLetter, isAlphaNum)
import Text.Read (readMaybe)

data Program = Program [Definition] Exp deriving (Show)

data Definition = Def String Exp
                | TAnn String QType
                | Struct QType [Type]
                -- | TDef QType Type
                -- | TSyn QType Type
                deriving (Eq, Show)

data QType = Forall [String] Type deriving (Eq, Show)

type Type = SExp

data Exp = Var String

         | Num Int

         | EAnn Exp Type

         | Lambda [String] Exp
         | Let [(String, Exp)] Exp
         | Letrec String Exp Exp

         | Case Exp [(String, [String], Exp)]

         | Application Exp Exp
         deriving (Eq, Show)


-- External Interface --

fromFile :: String -> String -> Either String [SExp]
fromFile name input = mapLeft show $ parse fileParser name input


parseProgram :: String -> String -> Either String Program
parseProgram name input = fromFile name input >>= toProgram


parseModule :: String -> String -> Either String [Definition]
parseModule name input = fromFile name input >>= toModule

-- Transformer --

-- <progam> ::= <def> ... <exp>
--
-- <module> ::= <def> ...
--
-- <def>    ::= (define <var> <exp>)
--           |  (define (<var> <arg> <arg> ...) <exp>)
--
--           |  (type <var> <qtype>)
--
--           |  (struct <qtype> (<type> <type> ...))
--
--           |  --(type-def <qtype> <type>)
--           |  --(type-synonym <qtype> <type>)
--
-- <qtype>  ::= (<forall> (<var> ...) <type>)
--           |  (<forall> <var> <type>)
--           |  <type>
--
-- <type>   ::= <var>
--           |  (<type> ...)
--
-- <forall> ::= ∀ | V | forall
--
-- <exp>    ::= <var>
--
--           |  <nat>
--
--           |  (type <exp> <type>)
--
--           |  (<lam> (<arg> ...) <exp>)
--           |  (let ((<var> <exp>) ...) <exp>)
--           |  (letrec (<var> <exp>) <exp>)
--
--           |  (case <exp> ((<pat> <exp>) ...))
--
--           |  (<exp> <exp> ...)
--
-- <pat>    ::= <var>
--           |  (<var> <arg> ...)
--
-- <arg>    ::= _ | <var>
--
-- <lam>    ::= λ | lambda

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
toDef (Node [Leaf "define", n, v]) = Def <$> toIdent n <*> toExpr v
toDef a@(Node (Leaf "type" : _)) = toAnn a
toDef s@(Node (Leaf "struct" : _)) = toStruct s
-- toDef td@(Node (Leaf "type-def" : _)) = toTDef td
-- toDef ts@(Node (Leaf "type-synonym" : _)) = toTSyn ts
toDef bad = Left $ message "definition" bad


toAnn :: SExp -> Either String Definition
toAnn (Node [Leaf "type", Leaf name, typ]) = TAnn name <$> toQType typ
toAnn bad = Left $ message "type annotation" bad


toStruct :: SExp -> Either String Definition
toStruct (Node (Leaf "struct" : name : variants)) =
  Struct <$> toQType name <*> mapM toType variants
toStruct bad = Left $ message "struct" bad


-- toTDef :: SExp -> Either String Definition
-- toTDef (Node [Leaf "type-def", name, typ]) = TDef <$> toType name <*> toType typ
-- toTDef bad = Left $ message "type-def" bad
--
--
-- toTSyn :: SExp -> Either String Definition
-- toTSyn (Node [Leaf "type-synonym", name, typ]) = TSyn <$> toType name <*> toType typ
-- toTSyn bad = Left $ message "type-synonym" bad


toQType :: SExp -> Either String QType
toQType t@(Node [Leaf kw, tvars, typ])
  | isForall = Forall <$> toTVars tvars <*> toType typ
  | otherwise = Forall [] <$> toType t
  where
    isForall = kw == "∀" || kw == "V" || kw == "forall"
    toTVars (Node vars) = mapM toIdent vars
    toTVars l@(Leaf _) = pure <$> toIdent l
toQType t = Forall [] <$> toType t

toType :: SExp -> Either String Type
toType l@(Leaf _) = Leaf <$> toIdent l
toType (Node ts) = Node <$> mapM toType ts


toExpr :: SExp -> Either String Exp
toExpr expr@(Leaf _) = tryBoth >>= ensurePositive
  where
    tryBoth = either (const (Var <$> toIdent expr)) Right $ toNum expr
    ensurePositive e@(Num n) | n < 0 = Left $ message "number" expr
                             | otherwise = Right e
    ensurePositive e = Right e
toExpr t@(Node (Leaf "type" : _)) = toEAnn t
toExpr lam@(Node (Leaf "lambda" : _)) = toLambda lam
toExpr lam@(Node (Leaf "λ" : _)) = toLambda lam
toExpr lt@(Node (Leaf "let" : _)) = toLet lt
toExpr letrec@(Node (Leaf "letrec" : _)) = toLetrec letrec
toExpr c@(Node (Leaf "case" : _)) = toCase c
toExpr (Node (f:xs)) = toExpr f >>= (\f' -> foldM doApp f' xs)
  where
    doApp f' arg = Application f' <$> toExpr arg
toExpr (Node []) = Left "Illegal empty expression"


toEAnn :: SExp -> Either String Exp
toEAnn (Node [Leaf "type", expr, typ]) = EAnn <$> toExpr expr <*> toType typ
toEAnn bad = Left $ message "type annotation" bad


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
toLetrec (Node [ Leaf "letrec", Node [Leaf name, expr], body]) =
  Letrec name <$> toExpr expr <*> toExpr body
toLetrec bad = Left $ message "letrec" bad


toCase :: SExp -> Either String Exp
toCase (Node (Leaf "case":e:cs@(_:_))) = Case <$> toExpr e <*> mapM clause cs
  where
    clause (Node [pat, expr]) = do
      (name, args) <- toPattern pat
      body <- toExpr expr
      return (name, args, body)
    clause bad = Left $ message "case clause" bad
    toPattern l@(Leaf _) = do { name <- toIdent l; return (name, []) }
    toPattern (Node (name : args)) = (,) <$> toIdent name <*> mapM toArg args
    toPattern bad = Left $ message "pattern" bad
toCase bad = Left $ message "case" bad


toNum :: SExp -> Either String Exp
toNum l@(Leaf num) = maybe die (Right . Num) $ readMaybe num
  where
    die = Left $ message "number" l
toNum bad = Left $ message "number" bad


toArg :: SExp -> Either String String
toArg (Leaf "_") = Right "_"
toArg arg = mapLeft (const (message "argument" arg)) $ toIdent arg


toIdent :: SExp -> Either String String
toIdent sexp@(Leaf name@(first:rest)) = if valid
                                        then Right name
                                        else Left $ message "identifier" sexp
  where
    valid = checkFirst first && all checkRest rest
    checkFirst c = isLetter c || Set.member c symbols
    checkRest c = isAlphaNum c || Set.member c symbols || c == '_'
    symbols = Set.fromList "!@#$%^&*+-=<>?/~"
toIdent bad = Left $ message "identifier" bad


message :: String -> SExp -> String
message s e = "Invalid " ++ s ++ " " ++ pretty e
