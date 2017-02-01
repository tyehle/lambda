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

walk :: ([Exp] -> a) -> Exp -> a
walk f (Var _) = f []

walk f VTrue = f []
walk f VFalse = f []
walk f (If a b c) = f [a, b, c]
walk f (And a b) = f [a, b]
walk f (Or a b) = f [a, b]
walk f (Not a) = f [a]

walk f (Num _) = f []
walk f (IsZero n) = f [n]
walk f (Minus a b) = f [a, b]
walk f (Plus a b) = f [a, b]
walk f (Mult a b) = f [a, b]
walk f (Divide a b) = f [a, b]
walk f (Eq a b) = f [a, b]
walk f (IsEven a) = f [a]

walk f (Lambda _ a) = f [a]
walk f (Let bindings body) = f $ map snd bindings ++ [body]
walk f (Letrec (_, (_, fnB)) body) = f [fnB, body]

walk f (Cons a b) = f [a, b]
walk f (Head a) = f [a]
walk f (Tail a) = f [a]
walk f (IsPair a) = f [a]
walk f (IsNull a) = f [a]
walk f VEmpty = f []

walk f (Application a b) = f [a, b]
