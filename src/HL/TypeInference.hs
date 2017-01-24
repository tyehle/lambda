module HL.TypeInference where

import HL.AST

data Type = TNum | TBool | TList Type | TFun [Type] Type deriving (Eq, Show)

infer :: Exp -> Maybe Type
infer VTrue  = Just TBool
infer VFalse = Just TBool
infer (If _ t f) = let tt = infer t; ft = infer f in if tt == ft then tt else Nothing
infer (And _ _) = Just TBool
infer (Or _ _) = Just TBool

infer (Num _) = Just TNum
infer (IsZero n) = inferUnOp TNum TBool n
infer (Minus a b) = inferBinOp TNum TNum a b
infer (Plus a b) = inferBinOp TNum TNum a b
infer (Mult a b) = inferBinOp TNum TNum a b
infer (Divide a b) = inferBinOp TNum TNum a b
infer (Eq a b) = inferBinOp TNum TBool a b

infer _ = Nothing


inferBinOp :: Type -> Type -> Exp -> Exp -> Maybe Type
inferBinOp expected result a b = if (infer a == Just expected) and (infer b == Just expected)
                                 then Just result
                                 else Nothing

inferUnOp :: Type -> Type -> Exp -> Maybe Type
inferUnOp expected result arg = if infer arg == Just expected
                                then Just result
                                else Nothing
