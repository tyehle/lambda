module HL.Base where

import HL.AST (Definition)
import HL.Typed (parseModule)
import HL.TypeConversion (convertModule)

import Control.Monad ((>=>))

baseFile :: String
baseFile = "resources/base.lc"

readBase :: IO (Either String [Definition])
readBase = (parseModule baseFile >=> convertModule) <$> readFile baseFile

errBase :: IO [Definition]
errBase = either error id <$> readBase
