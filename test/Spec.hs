import Test.Tasty
import Test.Tasty.HUnit

import ParserSpec
import HL.ParserSpec
import HL.CompilerSpec

import Node
import Lambda (interp, Result(..))

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ beta
  , parserTests
  , hlParserTests
  , compilerTests
  ]

nid :: String -> Result
nid arg = Clos [] arg $ Ref arg


beta :: TestTree
beta = testGroup "Beta Reduction"
  [ testCase "ref" $ interp [("a", nid "x")] (Ref "a") @?= nid "x"
  ]
