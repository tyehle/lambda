module Interpreter where

import Node

-- beta reduce a node
interp :: Node -> Node
interp n = let next = substInterp n
           in if next == n then n else interp next

substInterp :: Node -> Node
substInterp l@Lam{} = l
substInterp (Ref x) = error $ x ++ " not in scope"
substInterp (App f x) = case interp f of
                   Lam arg body -> doSubst arg (interp x) body
                   _ -> undefined

doSubst :: String -> Node -> Node -> Node
doSubst name val b@(Lam arg body) = if name == arg
                                       then b
                                       else Lam arg (doSubst name val body)
doSubst name val r@(Ref n) = if name == n then val else r
doSubst name val (App f' x') = App (doSubst name val f') (doSubst name val x')
