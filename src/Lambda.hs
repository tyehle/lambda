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

-- toCNum :: Integer -> Node
-- toCNum 0 = Lam "f" $ Lam "x" $ Ref "x"
-- toCNum n = Lam "f" $ Lam "x" $ App (Ref "f") (toCNum (n-1))
--
-- toCBool :: Bool -> Node
-- toCBool True  = Lam "a" $ Lam "b" $ Ref "a"
-- toCBool False = Lam "a" $ Lam "b" $ Ref "b"

-- fromCNum :: Node -> Integer
-- fromCNum n = undefined
--   where
--     extract env (Clos e arg body) =
