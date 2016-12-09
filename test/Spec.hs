import Test.Tasty
import Test.Tasty.HUnit

import ParserSpec
import HL.ParserSpec

import Node
import Lambda (interp)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ beta
  , parserTests
  , hlParserTests
  ]

nid :: String -> Node
nid arg = Lambda arg $ Ref arg


beta :: TestTree
beta = testGroup "Beta Reduction"
  [ testCase "ref" $ interp [("a", nid "x")] (Ref "a") @?= nid "x"
  ]
