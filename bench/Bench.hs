import Bollig
import Examples
import Teacher

import Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "NomNLStar"
    [ bench "NFA1 -" $ whnf (learnBollig 0 0) (teacherWithTargetNonDet 2 Examples.exampleNFA1)
    , bench "NFA2 1" $ whnf (learnBollig 0 0) (teacherWithTargetNonDet 3 (Examples.exampleNFA2 1))
    , bench "NFA2 2" $ whnf (learnBollig 0 0) (teacherWithTargetNonDet 4 (Examples.exampleNFA2 2))
    ]
  ]
