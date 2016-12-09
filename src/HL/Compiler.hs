module HL.Compiler (compile) where

import HL.AST
import Node (Node)
import Node as N


-- λx.λy.x
true :: Node
true = N.Lambda "x" $ N.Lambda "y" $ N.Ref "x"

-- λx.λy.y
false :: Node
false = N.Lambda "x" $ N.Lambda "y" $ N.Ref "y"

compile :: Exp -> Node
compile = undefined
