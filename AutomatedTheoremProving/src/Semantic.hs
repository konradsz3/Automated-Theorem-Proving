{-# LANGUAGE LambdaCase #-}

module Semantic where

import Parser
import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

evalFormula :: [(String, Bool)] -> Formula -> Bool
evalFormula env = \case
    Var x       -> fromMaybe False (lookup x env)
    Const b     -> b
    And a b     -> evalFormula env a && evalFormula env b
    Or a b      -> evalFormula env a || evalFormula env b
    Not a       -> not (evalFormula env a)
    Implies a b -> not (evalFormula env a) || evalFormula env b
    Equivalent a b -> (not (evalFormula env a) || evalFormula env b) && (not (evalFormula env b) || evalFormula env a)

data Context = Context
    { axioms :: Map.Map String Formula
    , givens :: [Formula]
    , goals  :: [Formula]
    } deriving (Show, Eq)

provable :: Context -> Formula -> Bool
provable ctx f =
    let env = [(v, True) | Var v <- givens ctx]
    in any (\assignment -> evalFormula assignment f) (allEnvs (allVars (givens ctx ++ Map.elems (axioms ctx))))
  where
    allVars :: [Formula] -> [String]
    allVars = foldr (\x acc -> case x of
        Var v   -> v : acc
        And a b -> allVars [a, b] ++ acc
        Or a b  -> allVars [a, b] ++ acc
        Not a   -> allVars [a] ++ acc
        Implies a b -> allVars [a, b] ++ acc
        Equivalent a b -> allVars [a, b] ++ acc
        _      -> acc) []

    allEnvs :: [String] -> [[(String, Bool)]]
    allEnvs [] = [[]]
    allEnvs (v:vs) = [ (v, b) : env | b <- [False, True], env <- allEnvs vs ]

emptyContext :: Context
emptyContext = Context Map.empty [] []

type ProofM = StateT Context (Either String)


evalStatement :: Statement -> ProofM ()
evalStatement = \case
    Axiom name f -> modify $ \ctx -> ctx { axioms = Map.insert name f (axioms ctx) }
    Theorem _ _  -> return ()
    Given f      -> modify $ \ctx -> ctx { givens = f : givens ctx }
    Assert f     -> do
        ctx <- get
        unless (provable ctx f) $
            lift $ Left $ "Cannot assert unprovable formula: " ++ show f
    Apply name args -> do
        ctx <- get
        case Map.lookup name (axioms ctx) of
            Nothing -> lift $ Left $ "Unknown axiom: " ++ name
            Just rule -> do
                let inst = head args
                unless (provable ctx inst) $
                    lift $ Left $ "Cannot apply " ++ name ++ ": premise not provable"
                modify $ \c -> c { givens = inst : givens c }
    ByContradiction steps -> do
        ctx <- get
        let assumed = Not (head (givens ctx))
        modify $ \c -> c { givens = assumed : givens c }
        mapM_ evalStatement steps
        ctx' <- get
        unless (Const False `elem` givens ctx') $
            lift $ Left "Contradiction not derived"

    Qed -> return ()

evalProgram :: Program -> Either String ()
evalProgram (Program stmts) = evalStateT (mapM_ evalStatement stmts) emptyContext
