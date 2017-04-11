module HL.Base where

import HL.AST (Definition)
import HL.Typed (parseModule)
import HL.TypeInference

import Control.Monad ((>=>))

baseFile :: String
baseFile = "resources/base.lc"

readBase :: IO (Either String [Definition])
readBase = (parseModule baseFile >=> inferModule) <$> readFile baseFile

errBase :: IO [Definition]
errBase = either error id <$> readBase
