{-# language ExistentialQuantification #-}
import Angluin
import Bollig
import Examples
import ObservationTable
import Teacher

import NLambda
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
data A = forall q i . (LearnableAlphabet i, Read i, NominalType q, Show q) => A (Automaton q i)

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
    let h = case read learnerName of
            NomLStar    -> learnAngluinRows teacher
            NomLStarCol -> learnAngluin teacher
            NomNLStar   -> learnBollig 0 0 teacher
    print h

mainWithIO :: String -> IO ()
mainWithIO learnerName = do
    let t = teacherWithIO atoms
    let h = case read learnerName of
            NomLStar    -> learnAngluinRows t
            NomLStarCol -> learnAngluin t
            NomNLStar   -> learnBollig 0 0 t
    print h

main :: IO ()
main = do
    bla <- getArgs
    case bla of
        [learnerName, teacherName, autName] -> mainExample learnerName teacherName autName
        [learnerName] -> mainWithIO learnerName
        _ -> putStrLn "Give either 1 (for the IO teacher) or 3 (for automatic teacher) arguments"