import           Angluin
import           Bollig
import           Examples
import           Teacher
import           NLStar

import           Data.IORef (readIORef)

main :: IO ()
main = do
    let h = learnAngluin (countingTeacher $ teacherWithTarget (Examples.fifoExample 3))
    putStrLn "Finished! Final hypothesis ="
    print h
    eqs <- readIORef eqCounter
    mqs <- readIORef mqCounter
    putStrLn "Number of equivalence queries:"
    print eqs
    putStrLn "Number of membership queries (and sizes+supports):"
    print (length mqs)
    print mqs
