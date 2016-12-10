import Test.Tasty

import ParserSpec
import HL.ParserSpec
import HL.CompilerSpec
import InterpreterSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ parserTests
  , hlParserTests
  , compilerTests
  , interpTests
  ]
