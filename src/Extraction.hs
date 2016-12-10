module Extraction where

import Node

data Aug = ALam String Aug
         | ARef String
         | AApp Aug Aug
         | ANum Integer
         | ABool Bool
         | AFun (Result -> Result)

type Env = [(String, Result)]
data Result = RClos Env String Aug
            | RFun (Result -> Result)
            | RNum Integer
            | RBool Bool

data InterpError = InterpError String deriving (Show)

toAug :: Node -> Aug
toAug (Lam arg body) = ALam arg (toAug body)
toAug (Ref x) = ARef x
toAug (App f x) = toAug f `AApp` toAug x

interpAug :: Env -> Aug -> Either InterpError Result
interpAug e (ALam x body) = Right $ RClos e x body
interpAug e (ARef x) = maybe (Left $ InterpError $ x ++ " not in scope") Right (lookup x e)
interpAug _ (ANum n) = Right $ RNum n
interpAug _ (ABool b) = Right $ RBool b
interpAug _ (AFun f) = Right $ RFun f
interpAug e (AApp f x) = interpAug e f >>= doApp
  where
    doApp (RClos e' arg body) = interpAug e x >>= \v -> interpAug ((arg, v):e') body
    doApp (RFun rf) = rf <$> interpAug e x
    doApp _ = Left . InterpError $ "Cannot apply numbers or booleans"

extractInt :: Node -> Either InterpError Integer
extractInt expr = case interpAug [] aug of
  Right (RNum n) -> Right n
  Right _ -> Left $ InterpError "Non-integer result"
  Left (InterpError msg) -> Left $ InterpError $ "Interpretation Error: " ++ msg
  where
    plus1 (RNum n) = RNum (n + 1)
    plus1 _        = undefined
    aug = toAug expr `AApp` AFun plus1 `AApp` ANum 0
