module Parser.ParserSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (liftM)
import Text.Parsec (ParseError, parse)

import Parser


unitTests = [testCase "Parse axiom" test_axiomStmt,
             testCase "Parse theorem" test_theorem,
             testCase "Parse given" test_given
            , testCase "Parse byContradiction" test_byContradictionStmt
            , testCase "Parse qed" test_qedStmt]

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

test_theorem :: Assertion 
test_theorem = parseTest parseProgram "THEOREM simple_implication: (P → Q) → (¬Q → ¬P);" (Program [Theorem "simple_implication" (Implies (Implies (Var "P") (Var "Q")) (Implies (Not (Var "Q")) (Not (Var "P"))))])

test_given :: Assertion 
test_given = parseTest parseProgram "GIVEN P → Q;" (Program [Given (Implies (Var "P") (Var "Q"))])

test_byContradictionStmt :: Assertion
test_byContradictionStmt = parseTest parseProgram "BY CONTRADICTION { AXIOM MyAxiom : (A ∧ B → C); }" (Program [ByContradiction [Axiom "MyAxiom" (Implies (And (Var "A") (Var "B")) (Var "C") )]])

test_qedStmt :: Assertion
test_qedStmt = parseTest parseProgram "QED;" (Program [Qed])
