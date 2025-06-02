module Parser.ParserSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (liftM)
import Text.Parsec (ParseError, parse)

import Parser

unitTests = [testCase "Parse axiom" test_axiomStmt
             , testCase "Parse assert" test_assertStmt
             , testCase "Parse apply" test_applyStmt]

propertyTests = []

tests =
    [ testGroup "Unit tests" unitTests
     , testGroup "Property-based tests" propertyTests
    ]

parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected =
    case parser input of
        Left err -> assertFailure $ "Parse error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

test_axiomStmt :: Assertion
test_axiomStmt = parseTest parseProgram "AXIOM MyAxiom : (A ∧ B);" (Program [Axiom "MyAxiom" (And (Var "A") (Var "B"))])

test_assertStmt :: Assertion
test_assertStmt = parseTest parseProgram "ASSERT A ∨ B;" (Program [Assert (Or (Var "A") (Var "B"))])

test_applyStmt :: Assertion 
test_applyStmt = parseTest parseProgram "APPLY contrapositive TO (P → Q) GET (¬Q → ¬P);" (Program [Apply "contrapositive" [Implies (Var "P") (Var "Q"), Implies (Not (Var "Q")) (Not (Var "P"))]])