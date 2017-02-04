module HL.Scoper where

import Data.Set (Set)
import qualified Data.Set as Set

import HL.AST


----- Scoping Errors -----

genErr :: Set String -> a -> Either String a
genErr vars good = maybe (Right good) (Left . msg) $ toMaybe vars
  where
    msg name = "Variable not in scope: " ++ name
    toMaybe = foldr (\e _ -> Just e) Nothing

scopeProgram :: Program -> Either String Program
scopeProgram prog@(Program ds e) = genErr (progFreeVars [] ds e) prog

scopeExp :: Exp -> Either String Exp
scopeExp expr = genErr (freeVars expr) expr


----- Find Free Variables -----

progFreeVars :: [String] -> [Definition] -> Exp -> Set String
progFreeVars env [] expr = freeVars expr `removeAll` env
progFreeVars env (Def name body : defs) expr = (freeVars body `removeAll` newEnv) `Set.union` progFreeVars newEnv defs expr
  where
    newEnv = name : env

freeVars :: Exp -> Set String
freeVars (Var name) = Set.singleton name
freeVars (Lambda args body) = freeVars body `removeAll` args
freeVars (Let bindings body) = foldr (Set.union . freeVars . snd) bodyVars bindings
  where
    bodyVars = freeVars body `removeAll` map fst bindings
freeVars (Letrec name binding body) = Set.delete name $ freeVars binding `Set.union` freeVars body
freeVars expr = walk (Set.unions . map freeVars) expr

removeAll :: Ord a => Set a -> [a] -> Set a
removeAll = foldr Set.delete
