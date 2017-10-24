import           Angluin
import           Bollig
import           Examples
import           Teacher
import           ObservationTable
import           NLStar

import           System.Environment
import           System.IO
import           NLambda

data Learner = NomLStar | NomLStarCol | NomNLStar
  deriving (Show, Read)

learn alphSet = do
    [learnerName] <- getArgs
    let t = teacherWithIO2 alphSet
    let h = case read learnerName of
            NomLStar    -> learnAngluinRows t
            NomLStarCol -> learnAngluin t
            NomNLStar   -> learnBollig t
    hPrint stderr h

main :: IO ()
main = do
    [learnerName] <- getArgs
    putStrLn "ALPHABET" -- ask for the alphabet from the teacher
    hFlush stdout
    alph <- getLine
    case alph of
        "ATOMS" -> learn atoms
        "FIFO" -> learn (NLambda.map Put atoms `union` NLambda.map Get atoms)
        _ -> error $ "Unknown alphabet " ++ alph
