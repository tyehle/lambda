module HL.Compiler where

import HL.AST
import Node

compile :: Exp -> Node
compile (Var name) = Ref name

compile VTrue = true
compile VFalse = false
-- (c (λy.t) (λy.f)) λx.x
-- make sure to test an exception in the clauses
compile (If cond t f) = (compile cond `App` Lam "y" (compile t) `App` Lam "y" (compile f)) `App` Lam "x" (Ref "x")
compile (And a b) = compile $ If a b VFalse
compile (Or a b) = compile $ If a VTrue b

compile (Num n) = churchNum n
compile (Test0 n) = compile n `App` Lam "x" false `App` true
compile (Minus a b) = undefined
compile (Plus a b) = undefined
compile (Mult a b) = undefined
compile (Eq a b) = compile $ And (Test0 (Minus a b)) (Test0 (Minus b a))

compile (Lambda args body) = foldr Lam (compile body) args
compile (Let [] body) = compile body -- might not be right
compile (Let ((n,v):rest) body) = Lam n (compile (Let rest body)) `App` compile v
compile (Letrec (name, fn) body) = undefined
-- let fn = \arg.( ... fn ... )
-- let fn = (\rec. \arg.( ... rec ... )) fn
-- let fn = (\rec. \arg.( ... rec ... )) (\rec. \arg.( ... rec ... ))
-- let fn = (\rec. \arg.( ... (rec rec) ... )) (\rec. \arg.( ... (rec rec) ... ))
-- let fn = \arg.( ... ((\rec. \arg.( ... (rec rec) ... )) (\rec. \arg.( ... (rec rec) ... ))) ... ))


compile (Cons a b) = undefined
compile (Head a) = undefined
compile (Tail a) = undefined
compile (TestPair a) = undefined
compile (TestNull a) = undefined
-- \a b. b (\x.x)
compile VEmpty = undefined

compile (Application a b) = compile a `App` compile b


-- λt.λf.t
true :: Node
true = Lam "t" $ Lam "f" $ Ref "t"

-- λt.λf.f
false :: Node
false = Lam "t" $ Lam "f" $ Ref "f"


churchNum :: Int -> Node
-- λf.λx.f^n x
churchNum n = Lam "f" $ Lam "x" $ foldr App (Ref "x") $ replicate n (Ref "f")
