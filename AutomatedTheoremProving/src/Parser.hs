{-# OPTIONS_GHC -w #-}
{-# LANGUAGE InstanceSigs #-}
module Parser where

import Text.Parsec
import Data.List (intercalate)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import System.IO (hSetEncoding, stdin, stdout, utf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- Abstract Syntax Tree definitions
-- Program representation
data Program = Program [Statement]
  deriving  (Eq)

-- Logical formulas (propositional logic only)
data Formula
  = Var String                     -- Variable
  | Const Bool                     -- Constant true/false
  | And Formula Formula            -- Logical AND
  | Or Formula Formula             -- Logical OR
  | Not Formula                    -- Logical NOT
  | Implies Formula Formula        -- Implication
  | Equivalent Formula Formula     -- Equivalence
  deriving Eq

-- Statements in the proof language
data Statement
  = Axiom String Formula           -- Define an axiom
  | Theorem String Formula         -- State a theorem
  | Given Formula                  -- Assumption for a proof
  | Assert Formula                 -- Claim without justification
  | Apply String [Formula]         -- Apply a rule or theorem
  | ByContradiction [Statement]    -- Proof by contradiction
  | Qed                            -- End of proof
  deriving Eq

instance Show Program where
  show (Program forms) = unlines (map show forms)

instance Show Formula where
  show :: Formula -> String
  show (Var v) = v
  show (Const True) = "True"
  show (Const False) = "False"
  show (And f1 f2) = "(" ++ show f1 ++ " \x2227 " ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ " \x2228 " ++ show f2 ++ ")"
  show (Not f) = "\x00AC " ++ "(" ++ show f ++ ")"
  show (Implies f1 f2) = "(" ++ show f1 ++ " \x2192 " ++ show f2 ++ ")"
  show (Equivalent f1 f2) = "(" ++ show f1 ++ " \x2194 " ++ show f2 ++ ")"

instance Show Statement where
  show (Axiom s f) = "AXIOM " ++ show s ++ ": " ++ show f ++ ";"
  show (Theorem s f) = "THEOREM " ++ show s ++ ": " ++ show f ++ ";"
  show (Given f) = "GIVEN " ++ show f ++ ";"
  show (Assert f) = "ASSERT " ++ show f ++ ";"
  show (Apply s fl) = "APPLY " ++ show s ++ " TO " ++ show (head fl) ++ " GET " ++ show (fl!!1) ++ ";"
  show (ByContradiction sl) = "BY CONTRADICTION " ++ "{" ++ indent (unlines (map show sl)) ++ "}"++ ";"
  show Qed = "QED;"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentLine = "//"
      , Token.reservedNames =
          [ "AXIOM", "THEOREM", "GIVEN", "ASSERT", "APPLY"
          , "BY", "CONTRADICTION", "QED", "True", "False"
          , "TO", "GET"
          ]
      , Token.reservedOpNames =
          [ "\x2227", "\x2228", "\x2192", "\x00AC", "\x2194", "(", ")", ":"
          ]
      }

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

cons :: Parser Bool
cons = (True  <$ reserved "True" )
    <|> (False <$ reserved "False")

term :: Parser Formula
term = parens formula
    <|> Const <$> cons
    <|> Var <$> identifier
    <|> Not <$> (reservedOp "\x00AC" *> term)

formula :: Parser Formula
formula = whiteSpace *> buildExpressionParser operators term
  where
    operators =
      [ [Prefix (reservedOp "\x00AC" >> return Not)]
      , [Infix  (reservedOp "\x2227" >> return And) AssocLeft]
      , [Infix  (reservedOp "\x2228" >> return Or) AssocLeft]
      , [Infix  (reservedOp "\x2192" >> return Implies) AssocRight]
      , [Infix  (reservedOp "\x2194" >> return Equivalent) AssocRight]
      ]

statement :: Parser Statement
statement = choice
    [ axiomStmt
      , theoremStmt
      , givenStmt
      , assertStmt
      , applyStmt
      , byContradictionStmt
      , qedStmt
    ] <* optional semi

axiomStmt :: Parser Statement
axiomStmt = do
    reserved "AXIOM"
    name <- identifier
    reservedOp ":"
    Axiom name <$> formula

assertStmt :: Parser Statement
assertStmt = do
    reserved "ASSERT"
    Assert <$> formula


applyStmt :: Parser Statement
applyStmt = do
    reserved "APPLY"
    rule <- identifier
    reserved "TO"
    firstFormula <- formula
    reserved "GET"
    secondFormula <- formula
    return (Apply rule [firstFormula, secondFormula])


theoremStmt :: Parser Statement
theoremStmt = do
    reserved "THEOREM"
    name <- identifier
    reservedOp ":"
    Theorem name <$> formula

givenStmt :: Parser Statement
givenStmt = do
  reserved "GIVEN"
  Given <$> formula


byContradictionStmt :: Parser Statement
byContradictionStmt = do
    reserved "BY"
    reserved "CONTRADICTION"
    stmts <- braces (statement `sepBy` semi)
    return (ByContradiction stmts)

qedStmt :: Parser Statement
qedStmt = do
  reserved "QED"
  return Qed 

programParser :: Parser Program
programParser = do
    whiteSpace
    stmts <- many statement
    eof
    return $ Program stmts

parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""