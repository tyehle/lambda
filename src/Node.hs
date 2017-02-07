module Node where

import qualified Data.Set as Set

import Pretty
import Scope

data Node = Lam String Node
          | Ref String
          | App Node Node
          deriving (Show, Eq)

instance Scope Node where
  freeVars (Lam arg body) = Set.delete arg $ freeVars body
  freeVars (Ref name) = Set.singleton name
  freeVars (App f x) = freeVars f `Set.union` freeVars x

instance Pretty Node where
  pretty (Lam arg body) = "λ" ++ arg ++ "." ++ pretty body
  pretty (Ref x) = x
  pretty (App f x) = left f ++ " " ++ right x
    where
      wrap n = "(" ++ pretty n ++ ")"
      left l@Lam{} = wrap l
      left other   = pretty other
      right a@App{} = wrap a
      right l@Lam{} = wrap l
      right other   = pretty other

lispString :: Node -> String
lispString (Lam arg body) = "(λ (" ++ arg ++ ")" ++ lispString body ++ ")"
lispString (Ref x) = x
lispString (App f x) = "(" ++ lispString f ++ " " ++ lispString x ++ ")"
