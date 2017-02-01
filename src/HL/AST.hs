module HL.AST where

data Exp = Var String

         | VTrue
         | VFalse
         | If Exp Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp

         | Num Int
         | IsZero Exp
         | Minus Exp Exp
         | Plus Exp Exp
         | Mult Exp Exp
         | Divide Exp Exp
         | Eq Exp Exp
         | IsEven Exp

         | Lambda [String] Exp
         | Let [(String, Exp)] Exp
         | Letrec (String, ([String], Exp)) Exp

         | Cons Exp Exp
         | Head Exp
         | Tail Exp
         | IsPair Exp
         | IsNull Exp
         | VEmpty

         | Application Exp Exp
         deriving (Show)
