module HL.AST where

data Exp = Var String

         | Tr
         | Fls
         | If Exp Exp Exp
         | And Exp Exp
         | Or Exp Exp

         | Num Integer
         | Test0 Exp
         | Minus Exp Exp
         | Plus Exp Exp
         | Mult Exp Exp
         | Eq Exp Exp

         | Lambda Lam
         | Let [(String, Exp)] Exp
         | Letrec (String, Lam) Exp

         | Cons Exp Exp
         | Head Exp
         | Tail Exp
         | TestPair Exp
         | TestNull Exp
         | EmptyList

         | App Exp Exp
         deriving (Show)


data Lam = Lam [String] Exp deriving (Show)
