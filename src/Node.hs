module Node where

import Data.List (intercalate)

type Env = [(String, Node)]

data Node = Lambda String Node
          | Closure Env String Node
          | Ref String
          | App Node Node
          deriving (Show, Eq)

prettyPrint :: Node -> String
prettyPrint (Lambda arg body) = "λ" ++ arg ++ "." ++ prettyPrint body
prettyPrint (Closure env arg body) = prettyEnv env ++ "⊣ " ++ prettyPrint (Lambda arg body)
prettyPrint (Ref x) = x
prettyPrint (App f x) = "(" ++ prettyPrint f ++ ") " ++ prettyPrint x

prettyEnv :: Env -> String
prettyEnv [] = "ø"
prettyEnv e  = "[" ++ intercalate "," (map (\(n,v) -> n++"="++prettyPrint v) e) ++ "]"
