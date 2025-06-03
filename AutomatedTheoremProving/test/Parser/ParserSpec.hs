{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
            , testCase "Parse assert" test_assertStmt
            , testCase "Parse apply" test_applyStmt
            , testCase "Parse byContradiction" test_byContradictionStmt
            , testCase "Parse qed" test_qedStmt]

propertyTests = [testProperty "Random formulas can be parsed" prop_randomFormula]

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

test_assertStmt :: Assertion
test_assertStmt = parseTest parseProgram "ASSERT A ∨ B;" (Program [Assert (Or (Var "A") (Var "B"))])

test_applyStmt :: Assertion
test_applyStmt = parseTest parseProgram "APPLY contrapositive TO (P → Q) GET (¬Q → ¬P);" (Program [Apply "contrapositive" [Implies (Var "P") (Var "Q"), Implies (Not (Var "Q")) (Not (Var "P"))]])

test_byContradictionStmt :: Assertion
test_byContradictionStmt = parseTest parseProgram "BY CONTRADICTION { AXIOM MyAxiom : (A ∧ B → C); }" (Program [ByContradiction [Axiom "MyAxiom" (Implies (And (Var "A") (Var "B")) (Var "C") )]])

test_qedStmt :: Assertion
test_qedStmt = parseTest parseProgram "QED;" (Program [Qed])

genIdentifier :: Gen String
genIdentifier = do
    first <- elements $ ['a' .. 'z']
    rest <- listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
    return (first : take 10 rest)

genBool :: Gen Bool
genBool = elements [True, False]

genFormula :: Int -> Gen Formula
genFormula 0 =
    oneof
        [ Var <$> genIdentifier
        , Const <$> genBool
        ]
genFormula n
    | n > 0 =
        let subformula = genFormula (n - 1)
         in frequency
                [ (4, Var <$> genIdentifier)
                , (3, Not <$> subformula)
                , (2, And <$> subformula <*> subformula)
                , (2, Or <$> subformula <*> subformula)
                , (1, Implies <$> subformula <*> subformula)
                , (1, Equivalent <$> subformula <*> subformula)
                , (1, Const <$> genBool)
                ]

prop_randomFormula :: Property 
prop_randomFormula = forAll (genFormula 2) $ \form ->
    let src = "AXIOM test: " ++ show form ++ ";"
        in case parseProgram src of
            Right _ -> True 
            Left _ -> False