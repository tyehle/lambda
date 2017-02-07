module Interpreter where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Scope
import Node

interp :: Node -> Either String Node
interp = Right . undoClosures . envInterp Map.empty

-- beta reduce a node
-- interp :: Node -> Node
-- interp n = let next = substInterp n
--            in if next == n then n else interp next
--
-- substInterp :: Node -> Node
-- substInterp l@Lam{} = l
-- substInterp (Ref x) = error $ x ++ " not in scope"
-- substInterp (App f x) = case interp f of
--                    Lam arg body -> doSubst arg (interp x) body
--                    _ -> undefined

doSubst :: String -> Node -> Node -> Node
doSubst name val b@(Lam arg body) = if name == arg
                                       then b
                                       else Lam arg (doSubst name val body)
doSubst name val r@(Ref n) = if name == n then val else r
doSubst name val (App f' x') = App (doSubst name val f') (doSubst name val x')


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
    letBind expr name inode | name `Set.member` freeVars expr = Lam name expr `App` undoClosures inode
                            | otherwise = expr
    -- reduceAndSubst expr name inode = doSubst name (undoClosures inode) expr
