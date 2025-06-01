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

-- Abstract Syntax Tree definitions
-- Program representation
data Program = Program [Statement]
  deriving (Show, Eq)

-- Logical formulas (propositional logic only)
data Formula
  = Var String                     -- Variable
  | Const Bool                     -- Constant true/false
  | And Formula Formula            -- Logical AND
  | Or Formula Formula             -- Logical OR
  | Not Formula                    -- Logical NOT
  | Implies Formula Formula        -- Implication
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

instance Show Formula where
  show :: Formula -> String
  show (Var v) = v
  show (Const True) = "True"
  show (Const False) = "False"
  show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
  show (Not f) = "¬" ++ show f
  show (Implies f1 f2) = "(" ++ show f1 ++ " → " ++ show f2 ++ ")"

instance Show Statement where
  show (Axiom s f) = "AXIOM " ++ show s ++ ": " ++ show f ++ ";"
  show (Theorem s f) = "THEOREM " ++ show s ++ ": " ++ show f ++ ";"
  show (Given f) = "GIVEN " ++ show f ++ ";"
  show (Assert f) = "ASSERT " ++ show f ++ ";"
  show (Apply s fl) = "APPLY " ++ show s ++ indent (unlines (map show fl)) ++ ";"
  show (ByContradiction sl) = "BY CONTRADICTION " ++ indent (unlines (map show sl)) ++ ";"
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
          , "BY CONTRADICTION", "QED"
          ]
      , Token.reservedOpNames =
          [ "∧", "∨", "→", "¬"
          ]
      }

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

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