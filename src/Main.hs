{-# LANGUAGE ExistentialQuantification #-}
import           Angluin
import           Bollig
import           Examples
import           Teacher
import           ObservationTable
import           NLStar

import           Data.IORef (readIORef)
import           System.Environment
import           NLambda

data Learner = NomLStar | NomLStarCol | NomNLStar
  deriving (Show, Read)

data Teacher = EqDFA | EqNFA Int
  deriving (Show, Read)

data Aut = Fifo Int | Stack Int | Running Int | NFA1 | Bollig Int
  deriving (Show, Read)

-- existential wrapper
data A = forall q i . (LearnableAlphabet i, NominalType q, Show q) => A (Automaton q i)

mainExample :: String -> String -> String -> IO ()
mainExample learnerName teacherName autName = do
    A automaton <- return $ case read autName of
            Fifo n    -> A $ Examples.fifoExample n
            Stack n   -> A $ Examples.stackExample n
            Running n -> A $ Examples.runningExample atoms n
            NFA1      -> A $ Examples.exampleNFA1
            Bollig n  -> A $ Examples.exampleNFA2 n
    let teacher = case read teacherName of
            EqDFA   -> teacherWithTarget automaton
            EqNFA k -> teacherWithTargetNonDet k automaton
    let h = case read learnerName of
            NomLStar    -> learnAngluinRows teacher
            NomLStarCol -> learnAngluin teacher
            NomNLStar   -> learnBollig teacher
    putStrLn "Finished! Final hypothesis ="
    print h

mainWithIO :: String -> IO ()
mainWithIO learnerName = do
    let h = case read learnerName of
            NomLStar    -> learnAngluinRows teacherWithIO
            NomLStarCol -> learnAngluin teacherWithIO
            NomNLStar   -> learnBollig teacherWithIO
    putStrLn "Finished! Final hypothesis ="
    print h

main :: IO ()
main = do
    bla <- getArgs
    case bla of
        [learnerName, teacherName, autName] -> mainExample learnerName teacherName autName
        [learnerName] -> mainWithIO learnerName
        _ -> putStrLn "Give either 1 (for the IO teacher) or 3 (for automatic teacher) arguments"
