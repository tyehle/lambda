module Node where

import Pretty

data Node = Lam String Node
          | Ref String
          | App Node Node
          deriving (Show, Eq)

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
