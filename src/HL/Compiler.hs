module HL.Compiler where

import HL.AST
import Node (Node)
import qualified Node as N

compile :: Exp -> Node
compile (Var name) = N.Ref name

compile Tr = true
compile Fls = false
-- (c (λy.t) (λy.f)) λx.x
-- make sure to test an exception in the clauses
compile (If cond t f) = (compile cond <> l "y" (compile t) <> l "y" (compile f)) <> l "x" (r "x")
compile (And a b) = compile $ If a b Fls
compile (Or a b) = compile $ If a Tr b

compile (Num n) = churchNum n
compile (Test0 n) = undefined
compile (Minus a b) = undefined
compile (Plus a b) = undefined
compile (Mult a b) = undefined
compile (Eq a b) = undefined

compile (Lambda (Lam args body)) = foldr N.Lambda (compile body) args
compile (Let bindings body) = undefined
compile (Letrec (name, fn) body) = undefined

compile (Cons a b) = undefined
compile (Head a) = undefined
compile (Tail a) = undefined
compile (TestPair a) = undefined
compile (TestNull a) = undefined
compile EmptyList = undefined

compile (App a b) = undefined


-- λx.λy.x
true :: Node
true = l "x" $ l "y" $ r "x"

-- λx.λy.y
false :: Node
false = l "x" $ l "y" $ r "x"


churchNum :: Integer -> Node
-- λf.λx.x
churchNum 0 = l "f" $ l "x" $ r "x"
-- λf.λx.x (churchNum (n-1))
churchNum n = l "f" $ l "x" $ r "f" <> churchNum (n-1)

l :: String -> Node -> Node
l = N.Lambda

r :: String -> Node
r = N.Ref

(<>) :: Node -> Node -> Node
a <> b = N.App a b
