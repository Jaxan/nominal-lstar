import Angluin
import Bollig
import Examples
import Teacher

import NLambda
import System.Environment
import System.IO

data Learner = NomLStar | NomLStarCol | NomNLStar
  deriving (Show, Read)

learn :: (Read i, Contextual i, NominalType i, Show i) => Set i -> IO ()
learn alphSet = do
    [learnerName] <- getArgs
    let t = teacherWithIO2 alphSet
    case read learnerName of
            NomLStar    -> hPrint stderr $ learnAngluinRows t
            NomLStarCol -> hPrint stderr $ learnAngluin t
            NomNLStar   -> hPrint stderr $ learnBollig 0 0 t

main :: IO ()
main = do
    putStrLn "ALPHABET" -- ask for the alphabet from the teacher
    hFlush stdout
    alph <- getLine
    case alph of
        "ATOMS" -> learn atoms
        "FIFO" -> learn (NLambda.map Put atoms `union` NLambda.map Get atoms)
        _ -> error $ "Unknown alphabet " ++ alph
