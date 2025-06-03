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
            , testCase "Parse qed" test_qedStmt
            , testCase "Parse error" test_invalidSyntax
            , testCase "Test snipet" test_ATPSnippet]

propertyTests = [testProperty "Random formulas can be parsed" prop_randomFormula
                , testProperty "Random axiom formula with name can be parsed" prop_axiomWithName
                , testProperty "Random apply formula with name can be parsed" prop_applyWithName
                , testProperty "Whitespace is insignificant" prop_whitespace
                , testProperty "Random theorem formula with name can be parsed" prop_theoremWithName
                , testProperty "Random given formula with name can be parsed" prop_givenWithName
                , testProperty "Random assert formula with name can be parsed" prop_assertWithName]

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
test_axiomStmt = parseTest parseProgram "AXIOM MyAxiom : (A ↔ B);" (Program [Axiom "MyAxiom" (Equivalent (Var "A") (Var "B"))])

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
    first <- elements ['A' .. 'Z']
    return [first]

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

genName :: Gen String
genName =
    resize 20 $
        listOf1 $
            elements $
                ['a' .. 'z'] ++ ['A' .. 'Z']

prop_axiomWithName :: Property
prop_axiomWithName = forAll genName $ \varName ->
    forAll (genFormula 3) $ \expr ->
        let
            stmt = Axiom varName expr 
            prog = Program [stmt]
            src = "AXIOM " ++ varName ++ ": " ++ show expr ++ ";"
         in
            case parseProgram src of
                Right p -> prog == p
                Left _ -> False

prop_applyWithName :: Property
prop_applyWithName = forAll genName $ \varName ->
    forAll (genFormula 3) $ \expr1 ->
    forAll (genFormula 3) $ \expr2 ->
        let
            stmt = Apply varName [expr1, expr2] 
            prog = Program [stmt]
            src = "APPLY " ++ varName ++ " TO " ++ show expr1 ++ " GET " ++ show expr2 ++ ";"
         in
            case parseProgram src of
                Right p -> prog == p
                Left _ -> False

prop_whitespace :: Property
prop_whitespace = forAll genName $ \varName ->
    forAll (genFormula 2 )$ \form ->
        let
            compact = "AXIOM " ++ varName ++ ": " ++ show form ++ ";"
            spaced = "AXIOM  " ++ varName ++ "  : " ++ show form ++ "  ;"
         in
            parseProgram compact === parseProgram spaced

prop_theoremWithName :: Property
prop_theoremWithName = forAll genName $ \varName ->
    forAll (genFormula 3) $ \expr ->
        let
            stmt = Theorem varName expr 
            prog = Program [stmt]
            src = "THEOREM " ++ varName ++ ": " ++ show expr ++ ";"
         in
            case parseProgram src of
                Right p -> prog == p
                Left _ -> False

prop_givenWithName :: Property
prop_givenWithName = forAll genName $ \varName ->
    forAll (genFormula 3) $ \expr ->
        let
            stmt = Given expr 
            prog = Program [stmt]
            src = "GIVEN " ++ show expr ++ ";"
         in
            case parseProgram src of
                Right p -> prog == p
                Left _ -> False

prop_assertWithName :: Property
prop_assertWithName = forAll genName $ \varName ->
    forAll (genFormula 3) $ \expr ->
        let
            stmt = Assert expr 
            prog = Program [stmt]
            src = "ASSERT " ++ show expr ++ ";"
         in
            case parseProgram src of
                Right p -> prog == p
                Left _ -> False

test_invalidSyntax :: Assertion
test_invalidSyntax = 
    case parseProgram "AXIOM x " of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Should have failed to parse incomplete Axiom"

test_ATPSnippet :: Assertion
test_ATPSnippet = do
    let program = unlines
            [ "AXIOM contrapositive: (P → Q) ↔ (¬Q → ¬P);"
            , "THEOREM simple_implication: P → Q → (¬Q → ¬P);"
            , "GIVEN P → Q;"
            , "APPLY contrapositive TO (P → Q) GET (¬Q → ¬P);"
            , "QED;"
            ]
    case parseProgram program of
        Right _ -> return ()
        Left err -> assertFailure $ "Failed to parse snippet: " ++ show err
