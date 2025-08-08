{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Semantics.SemanticsSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State
import qualified Data.Map as Map

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


statementUnitTests =
  [ testCase "Axiom adds to context" test_axiom
  , testCase "Given adds to context" test_given
  , testCase "Assert provable formula" test_assert_ok
  , testCase "Assert unprovable formula" test_assert_fail
  , testCase "Apply with known axiom and provable premise" test_apply_ok
  , testCase "Apply with unknown axiom" test_apply_unknown
  , testCase "Apply with unprovable premise" test_apply_fail
  , testCase "ByContradiction with contradiction" test_bycontradiction_ok
  , testCase "ByContradiction without contradiction" test_bycontradiction_fail
  , testCase "Qed leaves context unchanged" test_qed
  ]

runEval :: Statement -> Context -> Either String Context
runEval stmt = execStateT (evalStatement stmt)

test_axiom :: Assertion
test_axiom =
  let stmt = Axiom "ax1" (Var "P")
      Right ctx' = runEval stmt emptyContext
  in assertEqual "Axiom added" (Map.fromList [("ax1", Var "P")]) (axioms ctx')

test_given :: Assertion
test_given =
  let stmt = Given (Var "P")
      Right ctx' = runEval stmt emptyContext
  in assertEqual "Given added" [Var "P"] (givens ctx')

test_assert_ok :: Assertion
test_assert_ok =
  let stmt = Assert (Const True)
      ctx = emptyContext { givens = [Const True] }
  in assertEqual "Assert provable" (Right ctx) (runEval stmt ctx)

test_assert_fail :: Assertion
test_assert_fail =
  let stmt = Assert (Var "P")
      ctx = emptyContext
  in assertEqual "Assert unprovable"
       (Left "Cannot assert unprovable formula: P")
       (runEval stmt ctx)

test_apply_ok :: Assertion
test_apply_ok =
  let stmt = Apply "ax1" [Var "P", Var "Q"]
      ctx = emptyContext { axioms = Map.fromList [("ax1", Var "P")], givens = [Var "P"] }
      Right ctx' = runEval stmt ctx
  in assertBool "Premise added to givens" (Var "P" `elem` givens ctx')

test_apply_unknown :: Assertion
test_apply_unknown =
  let stmt = Apply "unknown" [Var "P", Var "Q"]
  in assertEqual "Apply unknown axiom"
       (Left "Unknown axiom: unknown")
       (runEval stmt emptyContext)

test_apply_fail :: Assertion
test_apply_fail =
  let stmt = Apply "ax1" [Var "Z"] 
      ctx = emptyContext { axioms = Map.fromList [("ax1", Var "P")] }
  in assertEqual "Apply unprovable premise"
       (Left "Cannot apply ax1: premise not provable")
       (runEval stmt ctx)

test_bycontradiction_ok :: Assertion
test_bycontradiction_ok =
  let stmt = ByContradiction [Given (Const False)]
      ctx = emptyContext { givens = [Var "P"] }
      Right ctx' = runEval stmt ctx
  in assertBool "Has negated assumption and False"
       (Not (Var "P") `elem` givens ctx' &&
        Const False `elem` givens ctx')

test_bycontradiction_fail :: Assertion
test_bycontradiction_fail =
  let stmt = ByContradiction []
      ctx = emptyContext { givens = [Var "P"] }
  in assertEqual "ByContradiction without contradiction"
       (Left "Contradiction not derived")
       (runEval stmt ctx)

test_qed :: Assertion
test_qed =
  let stmt = Qed
  in assertEqual "Qed leaves context unchanged"
       (Right emptyContext)
       (runEval stmt emptyContext)

tests =
    [ testGroup "Unit tests" unitTests
    , testGroup "Unit tests for evalStatement" statementUnitTests
    ]