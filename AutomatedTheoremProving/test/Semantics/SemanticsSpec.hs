{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Semantics.SemanticsSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (liftM)
import Text.Parsec (ParseError, parse)

import Semantic
import Parser

unitTests = [testCase "Evaluate true variable" test_evalvar1
            , testCase "Evaluate false variable" test_evalvar2
            , testCase "Evaluate true constant" test_evalconst1
            , testCase "Evaluate false constant" test_evalconst2
            , testCase "Evaluate true and statement" test_evaland1
            , testCase "Evaluate false and statement" test_evaland2
            , testCase "Evaluate true or statement" test_evalor1
            , testCase "Evaluate false or statement" test_evalor2
            , testCase "Evaluate true not statement" test_evalnot1
            , testCase "Evaluate false not statement" test_evalnot2
            , testCase "Evaluate true implies statement" test_evalimp1
            , testCase "Evaluate false implies statement" test_evalimp2
            , testCase "Evaluate true equivalent statement" test_evaleq1
            , testCase "Evaluate false equivalent statement" test_evaleq2
            ]

-- semanticsTest :: String -> Program -> Assertion 
-- semanticsTest input program = case evalProgram program of 
--     Left err -> assertFailure $ "Evaluation failed: " ++ err 
--     Right result -> assertEqual ("Evaluating: " ++ input) expected result

exampleEnv :: [(String, Bool)]
exampleEnv = [("X", True), ("Y", False)]

test_evalvar1 :: Assertion
test_evalvar1 = assertEqual "Var X" True (evalFormula exampleEnv (Var "X"))

test_evalvar2 :: Assertion
test_evalvar2 = assertEqual "Var Y" False (evalFormula exampleEnv (Var "Y"))

test_evalconst1 :: Assertion 
test_evalconst1 = assertEqual "Const True" True (evalFormula exampleEnv (Const True))

test_evalconst2 :: Assertion 
test_evalconst2 = assertEqual "Const False" False (evalFormula exampleEnv (Const False))

test_evaland1 :: Assertion
test_evaland1 = assertEqual "And Var X Var X" True (evalFormula exampleEnv (And (Var "X") (Var "X")))

test_evaland2 :: Assertion
test_evaland2 = assertEqual "And Var Y Var X" False (evalFormula exampleEnv (And (Var "Y") (Var "X")))

test_evalor1 :: Assertion
test_evalor1 = assertEqual "Or Var X Var Y" True (evalFormula exampleEnv (Or (Var "X") (Var "Y")))

test_evalor2 :: Assertion
test_evalor2 = assertEqual "Or Var Y Var Y" False (evalFormula exampleEnv (Or (Var "Y") (Var "Y")))

test_evalnot1 :: Assertion 
test_evalnot1 = assertEqual "Not Var X" False (evalFormula exampleEnv (Not (Var "X")))

test_evalnot2 :: Assertion 
test_evalnot2 = assertEqual "Not Var Y" True (evalFormula exampleEnv (Not (Var "Y")))

test_evalimp1 :: Assertion 
test_evalimp1 = assertEqual "Implies Var Y Var X" True (evalFormula exampleEnv (Implies (Var "Y") (Var "X")))

test_evalimp2 :: Assertion 
test_evalimp2 = assertEqual "Implies Var X Var Y" False (evalFormula exampleEnv (Implies (Var "X") (Var "Y")))

test_evaleq1 :: Assertion
test_evaleq1 = assertEqual "Equivalent Var X Var X" True (evalFormula exampleEnv (Equivalent (Var "X") (Var "X")))

test_evaleq2 :: Assertion
test_evaleq2 = assertEqual "Equivalent Var X Var Y" False (evalFormula exampleEnv (Equivalent (Var "X") (Var "Y")))

provableUnitTests =
  [ testCase "True is always provable" test_trueAlwaysProvable
  , testCase "False without givens is not provable" test_falseNotProvable
  , testCase "(p or not p) is provable" test_excludedMiddle
  , testCase "p and q without givens is not provable" test_andWithoutGivens
  , testCase "Variable from givens is provable" test_givenProvable
  , testCase "(p implies p) is provable" test_implicationTautology
  , testCase "(p equivalent p) is provable" test_equivalenceTautology
  ]

test_trueAlwaysProvable :: Assertion
test_trueAlwaysProvable =
  assertEqual "Const True" True (provable emptyContext (Const True))

test_falseNotProvable :: Assertion
test_falseNotProvable =
  assertEqual "Const False" False (provable emptyContext (Const False))

test_excludedMiddle :: Assertion
test_excludedMiddle =
  let f = Or (Var "p") (Not (Var "p"))
  in assertEqual "p \x2228 not p" True (provable emptyContext f)

test_andWithoutGivens :: Assertion
test_andWithoutGivens =
  let f = And (Var "p") (Var "q")
  in assertEqual "p \x2227 q without givens" False (provable emptyContext f)

test_givenProvable :: Assertion
test_givenProvable =
  let ctx = emptyContext { givens = [Var "p"] }
  in assertEqual "Variable from givens" True (provable ctx (Var "p"))

test_implicationTautology :: Assertion
test_implicationTautology =
  let f = Implies (Var "p") (Var "p")
  in assertEqual "p \x2192 p" True (provable emptyContext f)

test_equivalenceTautology :: Assertion
test_equivalenceTautology =
  let f = Equivalent (Var "p") (Var "p")
  in assertEqual "p \x2194 p" True (provable emptyContext f)

tests =
  [ testGroup "Unit tests" unitTests
  , testGroup "Provable unit tests" provableUnitTests
  ]
