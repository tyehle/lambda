module HL.Compiler where

import HL.AST
import Node
import Scope

import Util (fstOf3)
import Data.List (sortOn, foldl')

compile :: [Definition] -> Program -> Either String Node
compile defs = fmap compileExp . checkScope . desugarDefs defs

checkScope :: (Scope a) => a -> Either String a
checkScope input = maybe (Right input) scopeError . toMaybe . freeVars $ input
  where
    toMaybe = foldr (\e _ -> Just e) Nothing

desugarDefs :: [Definition] -> Program -> Exp
desugarDefs baseDefs (Program defs expr) = desugarProgram newProg
  where
    newProg = Program (baseDefs ++ defs) expr

desugarProgram :: Program -> Exp
desugarProgram (Program ds e) = foldr defToLet e ds

defToLet :: Definition -> Exp -> Exp
defToLet (Def name expr) body = if isFree name body
                                then if isFree name expr
                                     then Letrec name expr body
                                     else Let [(name, expr)] body
                                else body
defToLet (Struct variants) body = undefined
  where
    sorted = sortOn fst variants

compileExp :: Exp -> Node
compileExp (Var name) = Ref name

compileExp (Num n) = churchNum n
compileExp (Lambda args body) = foldr Lam (compileExp body) args
compileExp (Let [] body) = compileExp body
compileExp (Let ((n,v):rest) body) = Lam n inner `App` compileExp v
  where
    inner = compileExp (Let rest body)
compileExp (Letrec name binding body) = compileExp recursiveLet
  where
    recursiveLet = Let [(name, y `Application` Lambda [name] binding)] body
compileExp (Case e clauses) = compileExp $ foldl' (\f c -> Application f (toLambda c)) e sorted
  where
    toLambda (_, [], body) = body
    toLambda (_, args, body) = Lambda args body
    sorted = sortOn fstOf3 clauses
compileExp (Application a b) = compileExp a `App` compileExp b


-- (λy.λF.F (λx.y y F x))(λy.λF.F (λx.y y F x))
y :: Exp
y = term `Application` term
  where
    term = Lambda ["y", "F"] $ Var "F" `Application` Lambda ["x"] innerApp
    innerApp = Var "y" `Application`
               Var "y" `Application`
               Var "F" `Application`
               Var "x"
-- let fn = \arg.( ... fn ... )
-- let fn = (\rec. \arg.( ... rec ... )) fn
-- let fn = (\rec. \arg.( ... rec ... )) (\rec. \arg.( ... rec ... ))
-- let fn = (\rec. \arg.( ... (rec rec) ... )) (\rec. \arg.( ... (rec rec) ... ))
-- let fn = \arg.( ... ((\rec. \arg.( ... (rec rec) ... )) (\rec. \arg.( ... (rec rec) ... ))) ... ))


-- λf.λx.f^n x
churchNum :: Int -> Node
churchNum n = Lam "f" $ Lam "x" $ foldr App (Ref "x") $ replicate n (Ref "f")
