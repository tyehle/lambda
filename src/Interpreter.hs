module Interpreter where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

import Scope
import Node

interp :: Node -> Either String Node
interp = Right . undoClosures . envInterp Map.empty

type Env = Map String INode

data INode = IClos Env String Node | IRef String | IApp INode INode

envInterp :: Env -> Node -> INode
envInterp env (Lam arg body) = IClos env arg body
envInterp env (Ref x) = fromMaybe (IRef x) (Map.lookup x env)
envInterp env (App f x) = case envInterp env f of
  (IClos env' arg body) -> envInterp (Map.insert arg (envInterp env x) env') body
  complex -> IApp complex (envInterp env x)

undoClosures :: INode -> Node
undoClosures (IRef x) = Ref x
undoClosures (IApp f x) = App (undoClosures f) (undoClosures x)
undoClosures (IClos env arg body) = Lam arg $ Map.foldlWithKey' letBind body (Map.delete arg env)
  where
    letBind expr name inode | isFree name expr = Lam name expr `App` undoClosures inode
                            | otherwise = expr
