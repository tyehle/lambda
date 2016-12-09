module Node where

import Pretty

data Node = Lam String Node
          | Ref String
          | App Node Node
          deriving (Show, Eq)

instance Pretty Node where
  pretty (Lam arg body) = "λ" ++ arg ++ "." ++ pretty body
  pretty (Ref x) = x
  pretty (App f x) = "(" ++ pretty f ++ ") " ++ pretty x
