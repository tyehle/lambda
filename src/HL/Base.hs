module HL.Base where

import HL.AST (Definition)
import HL.SExp (parseModule)

baseFile :: String
baseFile = "resources/base.lc"

readBase :: IO (Either String [Definition])
readBase = parseModule baseFile <$> readFile baseFile

errBase :: IO [Definition]
errBase = either error id <$> readBase
