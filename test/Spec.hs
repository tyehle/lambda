import Test.Tasty

import ParserSpec
import HL.ParserSpec
import HL.CompilerSpec
import InterpreterSpec
import HL.SExpSpec
import HL.TypedSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ parserTests
  , hlParserTests
  , compilerTests
  , interpTests
  , sExpTests
  , typedTests
  ]
