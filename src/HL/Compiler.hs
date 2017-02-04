module HL.Compiler where

import HL.AST
import Node


-- always succeed for now
errCompile :: Exp -> Either String Node
errCompile = Right . compile

compile :: Exp -> Node
compile (Var name) = Ref name

compile VTrue = true
compile VFalse = false
-- (c (λy.t) (λy.f)) λx.x
-- make sure to test an exception in the clauses
compile (If cond t f) = (compile cond `App` Lam "y" (compile t) `App` Lam "y" (compile f)) `App` Lam "x" (Ref "x")
compile (And a b) = compile $ If a b VFalse
compile (Or a b) = compile $ If a VTrue b
compile (Not a) = compile $ If a VFalse VTrue

compile (Num n) = churchNum n
compile (IsZero n) = compile n `App` Lam "x" false `App` true
compile (Minus a b) = sub `App` compile a `App` compile b
compile (Plus a b) = plus `App` compile a `App` compile b
compile (Mult a b) = mult `App` compile a `App` compile b
compile (Divide a b) = divide `App` compile a `App` compile b
compile (Eq a b) = compile $ And (IsZero (Minus a b)) (IsZero (Minus b a))
compile (IsEven a) = compile a `App` compile switch `App` true
  where
    switch = Lambda ["x"] $ If (Var "x") VFalse VTrue

compile (Lambda args body) = foldr Lam (compile body) args
compile (Let [] body) = compile body
compile (Let ((n,v):rest) body) = Lam n (compile (Let rest body)) `App` compile v
compile (Letrec name binding body) = compile $ Let [(name, y `Application` Lambda [name] binding)] body


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

divide :: Node
divide = Lam "n" $ Lam "m" $ compile (div1let (Var "n") (Var "m"))
  where
    -- (if (zero? diff) 0 (+ 1 (div1 diff m)))
    body = If (IsZero (Var "diff"))
              (Num 0)
              (Plus (Num 1)
                    (Var "div1" `Application` Var "diff" `Application` Var "m"))
    -- (let ([diff (- n m)]) body)
    minusBinding = Let [("diff", Minus (Var "n") (Var "m"))] body
    div1let n m = Letrec "div1"
                         (Lambda ["n", "m"] minusBinding)
                         (Var "div1" `Application` Plus (Num 1) n `Application` m)


-- λh.λt.λf.λe.f h t
cons :: Node
cons = Lam "h" $ Lam "t" $ Lam "f" $ Lam "e" $ Ref "f" `App` Ref "h" `App` Ref "t"

-- λf.λe.e (λd.d)
empty :: Node
empty = Lam "f" $ Lam "e" $ Ref "e" `App` Lam "d" (Ref "d")

-- λl.l (λh.λt.h) hang
hd :: Node
hd = Lam "l" $ Ref "l" `App` Lam "h" (Lam "t" $ Ref "h") `App` hang

-- λl.l (λh.λt.t) hang
tl :: Node
tl = Lam "l" $ Ref "l" `App` Lam "h" (Lam "t" $ Ref "t") `App` hang

-- λl.l (λf.true) (λe.false)
isPair :: Node
isPair = Lam "l" $ Ref "l" `App` Lam "f" true `App` Lam "e" false

-- λl.l (λf.false) (λe.true)
isEmpty :: Node
isEmpty = Lam "l" $ Ref "l" `App` Lam "f" false `App` Lam "e" true


-- (λu.u u) (λu.u u)
hang :: Node
hang = Lam "u" (Ref "u" `App` Ref "u") `App` Lam "u" (Ref "u" `App` Ref "u")
