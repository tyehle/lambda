module HL.TypeInference where

import qualified HL.AST as AST
import qualified HL.Typed as Typed

import Data.Maybe (catMaybes)

-- data Type = TNum | TFun Type Type | Defined String  deriving (Eq, Show)

-- data TypeAnnotation = IDK

-- infer :: Exp -> Maybe Type
-- infer (Var x) = undefined x
-- infer (Num _) = Just TNum
-- infer (Lambda args body) = TFun <$> undefined args <*> infer body
-- infer (Let bindings body) = undefined bindings >> infer body
-- infer (Letrec name binding body) = undefined name binding >> infer body
-- infer (Application f x) = undefined f x

inferModule :: [Typed.Definition] -> Either String [AST.Definition]
inferModule defs = catMaybes <$> mapM keepDefs defs
  where
    keepDefs (Typed.Def name expr) = Just . AST.Def name <$> inferExp expr
    keepDefs (Typed.Struct _ variants) = Just . AST.Struct <$> undefined variants
    keepDefs _ = Right Nothing

inferProgram :: Typed.Program -> Either String AST.Program
inferProgram (Typed.Program defs expr) = AST.Program <$> inferModule defs <*> inferExp expr

inferExp :: Typed.Exp -> Either String AST.Exp
inferExp (Typed.Var x) = Right $ AST.Var x
inferExp (Typed.Num n) = Right $ AST.Num n
inferExp (Typed.EAnn e _) = inferExp e
inferExp (Typed.Lambda args body) = AST.Lambda args <$> inferExp body
inferExp (Typed.Let bindings body) = AST.Let <$> mapM inferBinding bindings <*> inferExp body
  where
    inferBinding (name, value) = (\v -> (name, v)) <$> inferExp value
inferExp (Typed.Letrec name value body) = AST.Letrec name <$> inferExp value <*> inferExp body
inferExp (Typed.Case e clauses) = undefined e clauses
inferExp (Typed.Application f x) = AST.Application <$> inferExp f <*> inferExp x
