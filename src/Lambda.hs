module Lambda where

import Node

import Data.Maybe (fromMaybe)

-- beta reduce a node
interp :: Env -> Node -> Node
interp e (Lambda x body) = Closure e x body
interp e (Ref x) = fromMaybe (error $ x ++ " not in scope") (lookup x e)
interp _ c@Closure{} = c
interp e (App f x) = case interp e f of
                       (Closure e' arg body) -> interp ((arg, interp e x):e') body
                       _ -> undefined

toCNum :: Integer -> Node
toCNum 0 = Lambda "f" $ Lambda "x" $ Ref "x"
toCNum n = Lambda "f" $ Lambda "x" $ App (Ref "f") (toCNum (n-1))

toCBool :: Bool -> Node
toCBool True  = Lambda "a" $ Lambda "b" $ Ref "a"
toCBool False = Lambda "a" $ Lambda "b" $ Ref "b"

-- fromCNum :: Node -> Integer
-- fromCNum n = undefined
--   where
--     extract env (Closure e arg body) =
