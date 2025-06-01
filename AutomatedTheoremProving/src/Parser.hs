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
  show (Not f) = "\x00AC " ++ show f
  show (Implies f1 f2) = "(" ++ show f1 ++ " \x2192 " ++ show f2 ++ ")"
  show (Equivalent f1 f2) = "(" ++ show f1 ++ " \x2194 " ++ show f2 ++ ")"

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
          , "BY CONTRADICTION", "QED", "True", "False"
          ]
      , Token.reservedOpNames =
          [ "\x2227", "\x2228", "\x2192", "\x00AC", "\x2194", "(", ")"
          ]
      }

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

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

cons :: Parser Bool
cons = (True  <$ reserved "True" )
    <|> (False <$ reserved "False")

term :: Parser Formula
term = parens formula
    <|> Const <$> cons
    <|> Var <$> identifier

-- constFormula :: Parser Formula
-- constFormula = (reserved "True" >> return (Const True))
--             <|> (reserved "False" >> return (Const False))

-- varFormula :: Parser Formula
-- varFormula = Var <$> identifier

formula :: Parser Formula
formula = buildExpressionParser operators term
  where
    operators =
      [ [Prefix (reservedOp "\x00AC" >> return Not)]
      , [Infix  (reservedOp "\x2227" >> return And) AssocLeft]
      , [Infix  (reservedOp "\x2228" >> return Or) AssocLeft]
      , [Infix  (reservedOp "\x2192" >> return Implies) AssocRight]
      , [Infix  (reservedOp "\x2194" >> return Equivalent) AssocRight]
      ]

example :: String
example = "\x00AC A ∨ (B ∧ C) → D ↔ E"

testParse :: String -> IO ()
testParse input = case parse (formula <* eof) "" input of
  Left err  -> print err
  Right ast -> print ast

  
-- main :: IO ()
-- main = do
--   hSetEncoding stdout utf8
--   hSetEncoding stdin utf8
--   putStrLn "Wczytano:"
--   input <- readFile "input.txt"
--   case parse (whiteSpace >> formula <* eof) "" input of
--     Left err -> print err
--     Right f  -> print f

-- Statement parsers
-- statement :: Parser Statement
-- statement = choice 
--     [ thinkStmt
--     , eatStmt
--     , printStmt
--     , declareResourceStmt
--     , loopStmt
--     , spawnStmt
--     , lockAllStmt
--     , unlockAllStmt
--     , letStmt
--     , foreachStmt
--     , ifStmt
--     ] <* optional semi

-- thinkStmt :: Parser Statement
-- thinkStmt = do
--     reserved "think"
--     Think <$> expr

-- eatStmt :: Parser Statement
-- eatStmt = do
--     reserved "eat"
--     duration <- expr
--     reserved "resource"
--     resource1Name <- identifier
--     reserved "resource"
--     resource2Name <- identifier
--     return $ Eat duration (Resource resource1Name) (Resource resource2Name)

-- printStmt :: Parser Statement
-- printStmt = do
--     reserved "print"
--     -- Now we can parse any expression and handle string conversion during evaluation
--     PrintExpr <$> expr

-- declareResourceStmt :: Parser Statement
-- declareResourceStmt = do
--     reserved "declareResource"
--     DeclareResource <$> expr 

-- loopStmt :: Parser Statement
-- loopStmt = do
--     reserved "loop"
--     Loop <$> braces (many statement)

-- spawnStmt :: Parser Statement
-- spawnStmt = do
--     reserved "spawn"
--     processName <- expr
--     body <- braces (many statement)
--     return $ Spawn processName body

-- lockAllStmt :: Parser Statement
-- lockAllStmt = do
--     reserved "lockAll"
--     resources <- brackets (commaSep exprNoSpace)
--     variables <- brackets (commaSep identifier)
--     return $ LockAll resources variables
--   where
--     -- Define operators locally for this parser
--     exprNoSpace = buildExpressionParser operatorsNoSpace termNoSpace
--     -- Define the operators table for exprNoSpace
--     operatorsNoSpace = 
--       [ [Infix (reservedOp "++" >> return Concat) AssocLeft]
--       , [Infix (reservedOp "+" >> return Add) AssocLeft,
--          Infix (reservedOp "-" >> return Sub) AssocLeft]
--       , [Infix (reservedOp "%" >> return Mod) AssocLeft]
--       ]
--     termNoSpace = parens expr
--                <|> Var <$> identifier
--                <|> StringLit <$> stringLiteral
--                <|> IntLit <$> integer
--                <|> randExpr

-- unlockAllStmt :: Parser Statement
-- unlockAllStmt = do
--     reserved "unlockAll"
--     resources <- brackets $ commaSep resourceReference
--     return $ UnlockAll resources
--   where
--     resourceReference = try (do
--         reserved "resource"
--         Resource <$> identifier)
--       <|> (Resource <$> identifier)

-- letStmt :: Parser Statement
-- letStmt = do
--     reserved "let"
--     name <- identifier
--     reservedOp "="
--     value <- expr
--     return $ Let name value

-- foreachStmt :: Parser Statement
-- foreachStmt = do
--     reserved "foreach"
--     startVal <- integer
--     reserved "to"
--     endVal <- integer
--     reserved "as"
--     indexVar <- identifier
--     body <- braces (many statement)
--     return $ ForEach startVal endVal indexVar body

-- ifStmt :: Parser Statement
-- ifStmt = do
--     reserved "if"
--     condition <- expr
--     thenBlock <- braces (many statement)
--     elseBlock <- option [] (reserved "else" >> braces (many statement))
--     return $ If condition thenBlock elseBlock