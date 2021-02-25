{-# language ExistentialQuantification #-}
import Angluin
import Bollig
import Examples
import Teacher

import NLambda hiding (automaton)
import Prelude hiding (map)
import System.Environment

data Learner
  = NomLStar     -- nominal L* for nominal automata
  | NomLStarCol  -- nominal L* with counterexamples as columns (suffix closed)
  | NomNLStar    -- NL* for nominal automata, counterexamples as columns (suffix closed)
  deriving (Show, Read)

data Teacher
  = EqDFA         -- Automatic teacher with membership and equivalence (only for DFAs)
  | EqNFA Int     -- Automatic teacher with membership and bounded equivalence
  | EquivalenceIO -- Teacher with automatic membership but manual equivalence
  deriving (Show, Read)

data Aut = Fifo Int | Stack Int | Running Int | NFA1 | Bollig Int | NonResidual | Residual1 | Residual2
  deriving (Show, Read)

-- existential wrapper
data A = forall q i . (NominalType i, Contextual i, Show i, Read i, NominalType q, Show q) => A (Automaton q i)

{- HLINT ignore "Redundant $" -}
mainExample :: String -> String -> String -> IO ()
mainExample learnerName teacherName autName = do
    A automaton <- return $ case read autName of
            Fifo n      -> A $ Examples.fifoExample n
            Stack n     -> A $ Examples.stackExample n
            Running n   -> A $ Examples.runningExample atoms n
            NFA1        -> A $ Examples.exampleNFA1
            Bollig n    -> A $ Examples.exampleNFA2 n
            NonResidual -> A $ Examples.exampleNonResidual
            Residual1   -> A $ Examples.exampleResidual1
            Residual2   -> A $ Examples.exampleResidual2
    let teacher = case read teacherName of
            EqDFA         -> teacherWithTarget automaton
            EqNFA k       -> teacherWithTargetNonDet k automaton
            EquivalenceIO -> teacherWithTargetAndIO automaton
    case read learnerName of
            NomLStar    -> print $ learnAngluinRows teacher
            NomLStarCol -> print $ learnAngluin teacher
            NomNLStar   -> print $ learnBollig 0 0 teacher

mainWithIO :: String -> IO ()
mainWithIO learnerName = do
    let t = teacherWithIO atoms
    case read learnerName of
            NomLStar    -> print $ learnAngluinRows t
            NomLStarCol -> print $ learnAngluin t
            NomNLStar   -> print $ learnBollig 0 0 t

main :: IO ()
main = do
    bla <- getArgs
    case bla of
        [learnerName, teacherName, autName] -> mainExample learnerName teacherName autName
        [learnerName] -> mainWithIO learnerName
        _ -> help

help :: IO ()
help = do
  putStrLn "Usage (for automated runs)"
  putStrLn ""
  putStrLn "    nominal-lstar <learner> <teacher> <automaton>"
  putStrLn ""
  putStrLn "or (for manual runs)"
  putStrLn ""
  putStrLn "    nominal-lstar <learner>"
  putStrLn ""
  putStrLn $ "where <learner> is any of " ++ show learners ++ ", <teacher> is any of " ++ show teachers ++ ", and <automaton> is any of " ++ show automata ++ ". (Replace 3 with any number you wish.)"
  where
    learners = [NomLStar, NomLStarCol, NomNLStar]
    teachers = [EqDFA, EqNFA 3, EquivalenceIO]
    automata = [Fifo 3, Stack 3, Running 3, NFA1, Bollig 3, NonResidual, Residual1, Residual2]
