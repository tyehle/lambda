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
interp = fromResult . closInterp []
  where
    fromResult (Clos _ arg body) = Lam arg body

closInterp :: Env -> Node -> Result
closInterp e (Lam x body) = Clos e x body
closInterp e (Ref x) = fromMaybe (error $ x ++ " not in scope") (lookup x e)
closInterp e (App f x) = doApp $ closInterp e f
  where
    doApp (Clos e' arg body) = closInterp ((arg, closInterp e x):e') body


step :: Node -> Node
step l@Lam{} = l
step (Ref x) = error $ x ++ " not in scope"
step (App f x) = case step f of
                   Lam arg body -> doSubst arg (step x) body
                   _ -> undefined

doSubst :: String -> Node -> Node -> Node
doSubst name val b@(Lam arg body) = if name == arg
                                       then b
                                       else Lam arg (doSubst name val body)
doSubst name val r@(Ref n) = if name == n then val else r
doSubst name val (App f' x') = App (doSubst name val f') (doSubst name val x')
