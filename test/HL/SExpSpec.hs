module HL.SExpSpec (sExpTests) where

import HL.SExp
import Pretty

import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec (Parsec, parse, getInput)

sExpTests :: TestTree
sExpTests = testGroup "S-Expression tests"
  [ prettyTests
  , sExpParserTests
  , blockCommentTests
  , sepTests
  , matchingTests
  ]

prettyTests :: TestTree
prettyTests = testGroup "Pretty Tests"
 [ testCase "1" $ pretty (Leaf "leaf") @?= "leaf"
 , testCase "2" $ pretty (Node [Leaf "aaa", Leaf "bbb"]) @?= "(aaa bbb)"
 ]

testParse :: (Eq a, Show a) => Int -> Parsec String () a -> String -> a -> TestTree
testParse n parser input expected = testCase (show n) assertion
  where
    result = parse parser "test input" input
    assertion = either (assertFailure . show) (@?= expected) result

testParseLeftover :: Int -> Parsec String () a -> String -> String -> TestTree
testParseLeftover n parser = testParse n (parser >> getInput)

sExpParserTests :: TestTree
sExpParserTests = testGroup "S-Expression Parser Tests"
  [ testParse 1 sExp "asdf" $ Leaf "asdf"
  , testParse 2 sExp "(asdf)" $ Node [Leaf "asdf"]
  , testParse 3 sExp "()" $ Node []
  , testParse 4 sExp "( asdf )" $ Node [Leaf "asdf"]
  , testParse 5 sExp "(a (b) (c d))" $ Node [Leaf "a", Node [Leaf "b"], Node [Leaf "c", Leaf "d"]]
  , testParse 6 sExp "( #||# asdf \t #| |# qwer)" $ Node [Leaf "asdf", Leaf "qwer"]
  ]

blockCommentTests :: TestTree
blockCommentTests = testGroup "Block Comment Tests"
  [ testParseLeftover 1 blockComment "#||#a" "a"
  , testParseLeftover 2 blockComment "#| asdf |#b" "b"
  , testParseLeftover 3 blockComment "#| asdf #| asdf |# |#c" "c"
  , testParseLeftover 4 blockComment "#| | # | |#d" "d"
  ]

sepTests :: TestTree
sepTests = testGroup "Sep Tests"
  [ testParseLeftover 1 sep " " ""
  , testParseLeftover 2 sep "\t\n \r " ""
  , testParseLeftover 3 sep "  ; line comment \n asdf" "asdf"
  , testParseLeftover 4 sep " ; asdf \n #| block #||#|# ; line\rb" "b"
  ]

matchingTests :: TestTree
matchingTests = testGroup "Matching Tests"
  [ testParse 1 (matching '(') ")" ')'
  , testParse 2 (matching '[') "]" ']'
  , testParse 3 (matching '{') "}" '}'
  ]
