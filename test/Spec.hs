import Test.Tasty

import ParserSpec
import HL.ParserSpec
import HL.CompilerSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ parserTests
  , hlParserTests
  , compilerTests
  ]
