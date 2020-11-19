{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-partial-type-signatures #-}
import Angluin (learnAngluin)
import Examples
import Teacher

import Gauge.Main
import Gauge.Main.Options
import NLambda

{- TODO: choose test framework -}

myConfig = defaultConfig
  { quickMode = True
  , includeFirstIter = True
  , csvFile = Just "test.csv"
  }

main = defaultMainWith myConfig
  [ bench "DW2" $ whnf lstarCol (target (runningExample atoms 2)) ]

-- Some (polymorphic) abbreviations
lstarCol :: _ => Teacher i -> Automaton _ i
lstarCol = learnAngluin

target :: _ => Automaton q i -> Teacher i
target = teacherWithTarget
