module HL.Scoper (checkScope) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative ((<|>))

import HL.AST

checkScope :: Exp -> Either String Exp
checkScope expr = maybe (Right expr) Left $ ensure Set.empty expr

ensure :: Set String -> Exp -> Maybe String
ensure env (Var name) | Set.member name env = Nothing
                      | otherwise = Just $ "Variable not in scope: " ++ name

ensure env (Lambda args body) = ensure (extendEnv env args) body
ensure env (Let bindings body) = ensureAll env (map snd bindings)
                              <|> ensure (extendEnv env (map fst bindings)) body
ensure env (Letrec (name, (args, fnBody)) body) = ensure (extendEnv env (name:args)) fnBody
                                               <|> ensure (extendEnv env [name]) body

ensure env expr = walk (ensureAll env) expr

extendEnv :: Set String -> [String] -> Set String
extendEnv = foldr Set.insert

ensureAll :: Set String -> [Exp] -> Maybe String
ensureAll _ [] = Nothing
ensureAll env (e:es) = ensure env e <|> ensureAll env es
