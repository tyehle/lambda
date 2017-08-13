import Test.Tasty

import ParserSpec
import HL.ParserSpec
import HL.CompilerSpec
import InterpreterSpec
import HL.SExpSpec
import HL.TypedSpec
import HL.TypeInferenceSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ parserTests
  , hlParserTests
  , compilerTests
  , interpTests
  , sExpTests
  , typedTests
  -- , typeInferenceTests
  ]
