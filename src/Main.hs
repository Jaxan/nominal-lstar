import           Angluin
import           Bollig
import           Examples
import           Teacher
import           NLStar

main :: IO ()
main = do
    let h = learnAngluin (teacherWithTarget (Examples.fifoExample 3))
    putStrLn "Finished! Final hypothesis ="
    print h
