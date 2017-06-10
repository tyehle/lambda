module HL.TypeConversion where

import qualified HL.AST as AST
import qualified HL.Typed as Typed

import Data.Maybe (catMaybes)

convertModule :: [Typed.Definition] -> Either String [AST.Definition]
convertModule defs = catMaybes <$> mapM keepDefs defs
  where
    keepDefs (Typed.Def name expr) = Just . AST.Def name <$> convertExp expr
    -- keepDefs (Typed.Struct _ variants) = Just . AST.Struct <$> mapM v2s variants
    keepDefs _ = Right Nothing
    -- v2s (Leaf name) = Right (name, [])
    -- v2s (Node (Leaf name : args)) = Right (name, map firstName args)
    -- v2s bad = Left $ "Invalid variant " ++ show bad
    -- firstName (Leaf name) = name
    -- firstName (Node []) = "empty"
    -- firstName (Node (child:_)) = firstName child

convertProgram :: Typed.Program -> Either String AST.Program
convertProgram (Typed.Program defs expr) = AST.Program <$> convertModule defs <*> convertExp expr

convertExp :: Typed.Exp -> Either String AST.Exp
convertExp (Typed.Var x) = Right $ AST.Var x
convertExp (Typed.Num n) = Right $ AST.Num n
convertExp (Typed.EAnn e _) = convertExp e
convertExp (Typed.Lambda args body) = AST.Lambda args <$> convertExp body
convertExp (Typed.Let bindings body) = AST.Let <$> mapM convertBinding bindings <*> convertExp body
  where
    convertBinding (name, value) = (\v -> (name, v)) <$> convertExp value
convertExp (Typed.Letrec name value body) = AST.Letrec name <$> convertExp value <*> convertExp body
-- convertExp (Typed.Case e clauses) = AST.Case <$> convertExp e <*> mapM convertClause clauses
--   where
--     convertClause (name, args, body) = convertExp body >>= \b -> return (name, args, b)
convertExp (Typed.Application f x) = AST.Application <$> convertExp f <*> convertExp x
