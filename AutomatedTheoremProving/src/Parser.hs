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
  deriving (Show, Eq)

instance Show Formula where
  show :: Formula -> String
  show (Var v) = v
  show (Const True) = "True"
  show (Const False) = "False"
  show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
  show (Not f) = "¬" ++ show f
  show (Implies f1 f2) = "(" ++ show f1 ++ " → " ++ show f2 ++ ")"