module HL.Compiler where

import HL.AST
import Node
import Scope


compile :: Program -> Either String Node
compile = checkScope . compileExp . desugarProgram

checkScope :: (Scope a) => a -> Either String a
checkScope input = maybe (Right input) (Left . msg) . toMaybe . freeVars $ input
  where
    msg name = "Variable not in scope: " ++ name
    toMaybe = foldr (\e _ -> Just e) Nothing

desugarDefs :: [Definition] -> Program -> Exp
desugarDefs baseDefs (Program defs expr) = desugarProgram $ Program (baseDefs ++ defs) expr

desugarProgram :: Program -> Exp
desugarProgram (Program ds e) = foldr defToLet e ds
  where
    defToLet (Def name expr) body = if isFree name body
                                    then if isFree name expr
                                         then Letrec name expr body
                                         else Let [(name, expr)] body
                                    else body

compileExp :: Exp -> Node
compileExp (Var name) = Ref name

compileExp VTrue = true
compileExp VFalse = false
-- (c (λy.t) (λy.f)) λx.x
-- make sure to test an exception in the clauses
compileExp (If cond t f) = (compileExp cond `App` Lam "y" (compileExp t) `App` Lam "y" (compileExp f)) `App` Lam "x" (Ref "x")
compileExp (And a b) = compileExp $ If a b VFalse
compileExp (Or a b) = compileExp $ If a VTrue b
compileExp (Not a) = compileExp $ If a VFalse VTrue

compileExp (Num n) = churchNum n
compileExp (IsZero n) = compileExp n `App` Lam "x" false `App` true
compileExp (Minus a b) = sub `App` compileExp a `App` compileExp b
compileExp (Plus a b) = plus `App` compileExp a `App` compileExp b
compileExp (Mult a b) = mult `App` compileExp a `App` compileExp b
compileExp (Divide a b) = divide `App` compileExp a `App` compileExp b
compileExp (Eq a b) = compileExp $ And (IsZero (Minus a b)) (IsZero (Minus b a))
compileExp (IsEven a) = compileExp a `App` switch `App` true
  where
    switch = Lam "x" $ Ref "x" `App` false `App` true

compileExp (Lambda args body) = foldr Lam (compileExp body) args
compileExp (Let [] body) = compileExp body
compileExp (Let ((n,v):rest) body) = Lam n (compileExp (Let rest body)) `App` compileExp v
compileExp (Letrec name binding body) = compileExp $ Let [(name, y `Application` Lambda [name] binding)] body


compileExp (Cons a b) = cons `App` compileExp a `App` compileExp b
compileExp (Head a) = hd `App` compileExp a
compileExp (Tail a) = tl `App` compileExp a
compileExp (IsPair a) = isPair `App` compileExp a
compileExp (IsNull a) = isEmpty `App` compileExp a
-- \a b. b (\x.x)
compileExp VEmpty = empty

compileExp (Application a b) = compileExp a `App` compileExp b


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
divide = Lam "n" $ Lam "m" $ compileExp (div1let (Var "n") (Var "m"))
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
isPair = Lam "l" $ Ref "l" `App` Lam "h" (Lam "t" true) `App` Lam "e" false

-- λl.l (λf.false) (λe.true)
isEmpty :: Node
isEmpty = Lam "l" $ Ref "l" `App` Lam "h" (Lam "t" false) `App` Lam "e" true


-- (λu.u u) (λu.u u)
hang :: Node
hang = Lam "u" (Ref "u" `App` Ref "u") `App` Lam "u" (Ref "u" `App` Ref "u")
