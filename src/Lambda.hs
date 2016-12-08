module Lambda where

import Data.Foldable (find)
import Text.Parsec

type Env = [(String, Node)]
data Node = Lambda String Node | Closure Env String Node | Ref String | App Node Node deriving (Show)

-- beta reduce a node
interp :: Env -> Node -> Node
interp e (Lambda x body) = Closure e x body
interp e (Ref x) = case find (\(n, _) -> n == x) e of
  Just (_, value) -> value
  Nothing -> error $ x ++ " not in scope"
interp _ c@Closure{} = c
interp e (App f x) = let f' = interp e f; x' = interp e x
                     in case f' of
                       (Closure e' arg body) -> interp ((arg, x'):e') body
                       _ -> undefined

parseProgram :: String -> Node
parseProgram = either (error . show) id . parse nodeParser "input"

nodeParser :: Parsec String () Node
nodeParser = parens <|> lambda <|> reference <|> application
 where
   parens = char '(' *> nodeParser <* char ')'
   lambda = do
     _ <- char '\\'
     arg <- many1 letter
     _ <- char '.'
     body <- nodeParser
     return $ Lambda arg body
   reference = Ref <$> many1 letter
   application = undefined

toCNum :: Integer -> Node
toCNum 0 = Lambda "f" $ Lambda "x" $ Ref "x"
toCNum n = Lambda "f" $ Lambda "x" $ App (Ref "f") (toCNum (n-1))

toCBool :: Bool -> Node
toCBool True  = Lambda "a" $ Lambda "b" $ Ref "a"
toCBool False = Lambda "a" $ Lambda "b" $ Ref "b"

-- fromCNum :: Node -> Integer
-- fromCNum n = undefined
--   where
--     extract env (Closure e arg body) =
