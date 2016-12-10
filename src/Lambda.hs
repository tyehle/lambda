module Lambda where

import Node
import Pretty

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Env = [(String, Result)]
prettyEnv :: Env -> String
prettyEnv [] = "ø"
prettyEnv e  = "[" ++ intercalate "," (map (\(n,v) -> n++"="++pretty v) e) ++ "]"

data Result = Clos Env String Node deriving (Eq, Show)
instance Pretty Result where
  pretty (Clos env arg body) = prettyEnv env ++ "⊣ " ++ pretty (Lam arg body)

-- beta reduce a node
interp :: Node -> Node
interp n = let next = substInterp n
           in if next == n then n else interp next

closInterp :: Env -> Node -> Result
closInterp e (Lam x body) = Clos e x body
closInterp e (Ref x) = fromMaybe (error $ x ++ " not in scope") (lookup x e)
closInterp e (App f x) = doApp $ closInterp e f
  where
    doApp (Clos e' arg body) = closInterp ((arg, closInterp e x):e') body


substInterp :: Node -> Node
substInterp l@Lam{} = l
substInterp (Ref x) = error $ x ++ " not in scope"
substInterp (App f x) = case substInterp f of
                   Lam arg body -> doSubst arg (substInterp x) body
                   _ -> undefined

doSubst :: String -> Node -> Node -> Node
doSubst name val b@(Lam arg body) = if name == arg
                                       then b
                                       else Lam arg (doSubst name val body)
doSubst name val r@(Ref n) = if name == n then val else r
doSubst name val (App f' x') = App (doSubst name val f') (doSubst name val x')
