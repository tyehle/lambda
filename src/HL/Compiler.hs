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
compile (IsZero n) = compile n `App` Lam "x" false `App` true
compile (Minus a b) = sub `App` compile a `App` compile b
compile (Plus a b) = plus `App` compile a `App` compile b
compile (Mult a b) = mult `App` compile a `App` compile b
compile (Eq a b) = compile $ And (IsZero (Minus a b)) (IsZero (Minus b a))

compile (Lambda args body) = foldr Lam (compile body) args
compile (Let [] body) = compile body
compile (Let ((n,v):rest) body) = Lam n (compile (Let rest body)) `App` compile v
compile (Letrec (name, fn) body) = compile $ Let [(name, y `Application` Lambda [name] fnLambda )] body
  where
    fnLambda = uncurry Lambda fn


compile (Cons a b) = cons `App` compile a `App` compile b
compile (Head a) = hd `App` compile a
compile (Tail a) = tl `App` compile a
compile (IsPair a) = isPair `App` compile a
compile (IsNull a) = isEmpty `App` compile a
-- \a b. b (\x.x)
compile VEmpty = empty

compile (Application a b) = compile a `App` compile b


-- λt.λf.t
true :: Node
true = Lam "t" $ Lam "f" $ Ref "t"

-- λt.λf.f
false :: Node
false = Lam "t" $ Lam "f" $ Ref "f"


-- (λy.λF.F (λx.y y F x))(λy.λF.F (λx.y y F x))
y :: Exp
y = term `Application` term
  where
    term = Lambda ["y", "F"] $ Var "F" `Application` Lambda ["x"] innerApp
    innerApp = Var "y" `Application` Var "y" `Application` Var "F" `Application` Var "x"
-- let fn = \arg.( ... fn ... )
-- let fn = (\rec. \arg.( ... rec ... )) fn
-- let fn = (\rec. \arg.( ... rec ... )) (\rec. \arg.( ... rec ... ))
-- let fn = (\rec. \arg.( ... (rec rec) ... )) (\rec. \arg.( ... (rec rec) ... ))
-- let fn = \arg.( ... ((\rec. \arg.( ... (rec rec) ... )) (\rec. \arg.( ... (rec rec) ... ))) ... ))


-- λf.λx.f^n x
churchNum :: Int -> Node
churchNum n = Lam "f" $ Lam "x" $ foldr App (Ref "x") $ replicate n (Ref "f")

-- λn.n (λx.false) true
isZero :: Node
isZero = Lam "n" $ Ref "n" `App` Lam "x" false `App` true

-- λn.λm.λf.λx.(m f) ((n f) x)
plus :: Node
plus = Lam "n" $ Lam "m" $ Lam "f" $ Lam "x" $
        (Ref "m" `App` Ref "f") `App` ((Ref "n" `App` Ref "f") `App` Ref "x")

-- λn.λm.λf.m (n f)
mult :: Node
mult = Lam "n" $ Lam "m" $ Lam "f" $
        Ref "m" `App` (Ref "n" `App` Ref "f")

-- λn.λf.λx. n (λg.λh.h (g f)) (λu.x) (λu.u)
prev :: Node
prev = Lam "n" $ Lam "f" $ Lam "x" $
        Ref "n" `App` inner `App` Lam "u" (Ref "x") `App` Lam "u" (Ref "u")
  where
    -- λg.λh.h (g f)
    inner = Lam "g" $ Lam "h" $ Ref "h" `App` (Ref "g" `App` Ref "f")

-- λn.λm.m prev n
sub :: Node
sub = Lam "n" $ Lam "m" $ Ref "m" `App` prev `App` Ref "n"



cons :: Node
cons = undefined

empty :: Node
empty = undefined

hd :: Node
hd = undefined

tl :: Node
tl = undefined

isPair :: Node
isPair = undefined

isEmpty :: Node
isEmpty = undefined
