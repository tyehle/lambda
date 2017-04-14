module HL.AST where

import Scope

import Data.Set (Set)
import qualified Data.Set as Set

data Program = Program [Definition] Exp deriving (Show)

data Definition = Def String Exp
                | Struct [(String, [String])]
                deriving (Show)

data Exp = Var String

         | Num Int

         | Lambda [String] Exp
         | Let [(String, Exp)] Exp
         | Letrec String Exp Exp

         | Case Exp [(String, [String], Exp)]

         | Application Exp Exp
         deriving (Show)


instance Scope Program where
  freeVars (Program defs expr) = progFreeVars [] defs expr

progFreeVars :: [String] -> [Definition] -> Exp -> Set String
progFreeVars env [] expr = freeVars expr `removeAll` env
progFreeVars env (Struct variants : defs) expr = progFreeVars (map fst variants ++ env) defs expr
progFreeVars env (Def name body : defs) expr = theseVars `Set.union` recurVars
  where
    newEnv = name : env
    theseVars = freeVars body `removeAll` newEnv
    recurVars = progFreeVars newEnv defs expr

instance Scope Exp where
  freeVars (Var name) = Set.singleton name
  freeVars (Num _) = Set.empty
  freeVars (Lambda args body) = freeVars body `removeAll` args
  freeVars (Let bindings body) = foldr addBindingVars bodyVars bindings
    where
      addBindingVars = Set.union . freeVars . snd
      bodyVars = freeVars body `removeAll` map fst bindings
  freeVars (Letrec name binding body) = Set.delete name allVars
    where
      allVars = freeVars binding `Set.union` freeVars body
  freeVars (Case e clauses) = Set.unions $ freeVars e : map clauseVars clauses
    where
      clauseVars (_, args, body) = freeVars body `removeAll` args
  freeVars (Application f x) = freeVars f `Set.union` freeVars x

removeAll :: Ord a => Set a -> [a] -> Set a
removeAll = foldr Set.delete
