module AbstractLStar where

import NLambda

data TestResult i
    = Succes                     -- test succeeded, no changes required
    | Failed (Set [i]) (Set [i]) -- test failed, change: add rows + columns
