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
            | RPair (Result, Result)
            | REmpty

r2b :: Result -> Maybe Bool
r2b (RBool b) = Just b
r2b _ = Nothing

r2i :: Result -> Maybe Integer
r2i (RNum n) = Just n
r2i _ = Nothing

r2p :: Result -> Maybe (Result, Result)
r2p (RPair p) = Just p
r2p _ = Nothing

type InterpError = String

toAug :: Node -> Aug
toAug (Lam arg body) = ALam arg (toAug body)
toAug (Ref x) = ARef x
toAug (App f x) = toAug f `AApp` toAug x

interpAug :: Env -> Aug -> Either InterpError Result
interpAug e (ALam x body) = Right $ RClos e x body
interpAug e (ARef x) = maybe (Left $ x ++ " not in scope") Right (lookup x e)
interpAug _ (ANum n) = Right $ RNum n
interpAug _ (ABool b) = Right $ RBool b
interpAug _ (AFun f) = Right $ RFun f
interpAug e (AApp f x) = interpAug e f >>= doApp
  where
    doApp (RClos e' arg body) = interpAug e x >>= \v -> interpAug ((arg, v):e') body
    doApp (RFun rf) = rf <$> interpAug e x
    doApp _ = Left "Cannot apply numbers or booleans"

intExtractor :: Result -> Either InterpError Integer
intExtractor = undefined

extractInt :: Node -> Either InterpError Integer
extractInt expr = extractResult r2i $ toAug expr `AApp` AFun plus1 `AApp` ANum 0
  where
    plus1 (RNum n) = RNum (n+1)
    plus1 _ = undefined

extractBool :: Node -> Either InterpError Bool
extractBool expr = extractResult r2b $ toAug expr `AApp` ABool True `AApp` ABool False

extractPair :: Node -> Either InterpError (Result, Result)
extractPair expr = extractResult r2p $ toAug expr `AApp` AFun onCons `AApp` AFun onEmpty

extractResult :: (Result -> Maybe a) -> Aug -> Either InterpError a
extractResult extractor expr = case interpAug [] expr of
  res@Right{} -> res >>= maybe (Left "Wrong type in extraction") Right . extractor
  Left msg -> Left $ "Interpretation Error: " ++ msg

-- empty = \_ onNil -> onNil id
-- lst = \onCons _ -> onCons "hd" "tl"

onEmpty :: Result -> Result
onEmpty = const REmpty

onCons :: Result -> Result
onCons hd = RFun (\tl -> RPair (hd,tl))
