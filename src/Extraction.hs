module Extraction where

import Node

type InterpError = String
type Extractor a = Result -> Either InterpError a

data Aug = ALam String Aug
         | ARef String
         | AApp Aug Aug
         | ANum Integer
         | ABool Bool
         | AFun (Extractor Result)

type Env = [(String, Result)]
data Result = RClos Env String Aug
            | RFun (Extractor Result)
            | RNum Integer
            | RBool Bool
            | RPair (Result, Result)
            | REmpty

r2b :: Extractor Bool
r2b (RBool b) = Right b
r2b _ = Left "Non-boolean result"

r2i :: Extractor Integer
r2i (RNum n) = Right n
r2i _ = Left "Non-int result"

r2p :: Extractor (Result, Result)
r2p (RPair p) = Right p
r2p _ = Left "Non-pair result"

r2e :: Extractor [a]
r2e REmpty = Right []
r2e _ = Left "Non-empty result"

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
    doApp (RFun rf) = interpAug e x >>= rf
    doApp _ = Left "Cannot apply numbers or booleans"


toRes :: Node -> Either InterpError Result
toRes = interpAug [] . toAug

intExtractor :: Extractor Integer
intExtractor (RClos e arg body) = interpAug ((arg, RFun plus1):e) (body `AApp` ANum 0) >>= r2i
intExtractor r = r2i r

extractInt :: Node -> Either InterpError Integer
extractInt expr = toRes expr >>= intExtractor

boolExtractor :: Extractor Bool
boolExtractor (RClos e arg body) = interpAug ((arg, RBool True):e) (body `AApp` ABool False) >>= r2b
boolExtractor r = r2b r

extractBool :: Node -> Either InterpError Bool
extractBool expr = toRes expr >>= boolExtractor

pairExtractor :: Extractor (Result, Result)
pairExtractor (RClos e arg body) = interpAug ((arg, RFun onCons):e) (body `AApp` AFun onEmpty) >>= r2p
pairExtractor r = r2p r

extractPair :: Node -> Either InterpError (Result, Result)
-- extractPair expr = interpAug [] (toAug expr `AApp` AFun onCons `AApp` AFun onEmpty) >>= r2p
extractPair expr = toRes expr >>= pairExtractor

emptyExtractor :: Extractor [a]
emptyExtractor (RClos e arg body) = interpAug ((arg, RFun onCons):e) (body `AApp` AFun onEmpty) >>= r2e
emptyExtractor r = r2e r

extractEmpty :: Node -> Either InterpError [a]
extractEmpty expr = toRes expr >>= emptyExtractor

listExtractor :: Extractor a -> Result -> Either InterpError [a]
listExtractor ex (RClos e arg body) = interpAug ((arg, RFun onCons):e) (body `AApp` AFun onEmpty) >>= listExtractor ex
listExtractor ex (RPair (hd,tl)) = (:) <$> ex hd <*> listExtractor ex tl
listExtractor _ REmpty = Right []
listExtractor _ _ = Left "Non-list result"

extractList :: Extractor a -> Node -> Either InterpError [a]
extractList extractor expr = toRes expr >>= listExtractor extractor


plus1 :: Extractor Result
plus1 (RNum n) = Right $ RNum (n+1)
plus1 _ = Left "Addition on non-int type"

onEmpty :: Extractor Result
onEmpty = const $ Right REmpty

onCons :: Extractor Result
onCons hd = Right $ RFun (\tl -> Right $ RPair (hd,tl))
