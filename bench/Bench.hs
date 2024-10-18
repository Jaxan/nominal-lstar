{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-partial-type-signatures #-}
import Angluin (learnAngluin, learnAngluinRows)
import Bollig (learnBollig)
import Examples
import Teacher

import Test.Tasty.Bench
import NLambda

{- Benchmarks can be run with `cabal bench`. If you would like to run a
   subset of benchmarks, you can do this with `stack bench --ba NomNLStar`
   for example. May take a long time (so we only run once).

   Note that we are using the whitebox equivalence checks, which may not
   reflect "realistic" runtinme, because the representation of the
   counterexamples is different.

   The nondeterministic benchmarks are a bit subtle, because equivalence
   is not decidable. We do a bounded bisimulation check, the bounds are
   guessed to be sufficient, but not proven to be.

   Examples.example4 is not used, because it takes a bit too long.
-}

-- Some bounds on the size of automata
fifoBound = 4
stackBound = 4
doublewordBound = 3
nlastpositionBound = 4

main :: IO ()
main = defaultMain
  [ bgroup "NomLStarR"
    [ bgroup "Fifo" $
      fmap (\n -> bench (show n) $ whnf lstar (target (fifoExample n))) [0..fifoBound]
    , bgroup "Stack" $
      fmap (\n -> bench (show n) $ whnf lstar (target (stackExample n))) [0..stackBound]
    , bgroup "Double Word" $
      fmap (\n -> bench (show n) $ whnf lstar (target (runningExample atoms n))) [0..doublewordBound]
    , bgroup "n-last Position" $
      fmap (\n -> bench (show n) $ whnf lstar (targetNonDet (n+2) (exampleNFA2 n))) [0..nlastpositionBound]
    , bench "example1" $ whnf lstar (target example1)
    , bench "example2" $ whnf lstar (target example2)
    , bench "example3" $ whnf lstar (target example3)
    --, bench "example4" $ whnf lstar (target example4)
    , bench "example5" $ whnf lstar (target example5)
    ]
  , bgroup "NomLStarC"
    [ bgroup "Fifo" $
      fmap (\n -> bench (show n) $ whnf lstarCol (target (fifoExample n))) [0..fifoBound]
    , bgroup "Stack" $
      fmap (\n -> bench (show n) $ whnf lstarCol (target (stackExample n))) [0..stackBound]
    , bgroup "Double Word" $
      fmap (\n -> bench (show n) $ whnf lstarCol (target (runningExample atoms n))) [0..doublewordBound]
    , bgroup "n-last Position" $
      fmap (\n -> bench (show n) $ whnf lstarCol (targetNonDet (n+2) (exampleNFA2 n))) [0..nlastpositionBound]
    , bench "example1" $ whnf lstarCol (target example1)
    , bench "example2" $ whnf lstarCol (target example2)
    , bench "example3" $ whnf lstarCol (target example3)
    --, bench "example4" $ whnf lstarCol (target example4)
    , bench "example5" $ whnf lstarCol (target example5)
    ]
  , bgroup "NomNLStar"
    [ bgroup "n-last Position" $
      fmap (\n -> bench (show n) $ whnf nlstar (targetNonDet (n+2) (exampleNFA2 n))) [0..nlastpositionBound]
    , bench "example1" $ whnf nlstar (targetNonDet 3 example1)
    , bench "example2" $ whnf nlstar (targetNonDet 2 example2)
    , bench "example3" $ whnf nlstar (targetNonDet 2 example3)
    --, bench "example4" $ whnf nlstar (targetNonDet 4 example4)
    , bench "example5" $ whnf nlstar (targetNonDet 3 example5)
    , bench "NFA1    " $ whnf nlstar (targetNonDet 2 exampleNFA1)
    , bench "Residual" $ whnf nlstar (targetNonDet 2 exampleResidual1)
    ]
  ]

-- Some (polymorphic) abbreviations
lstar, lstarCol, nlstar :: _ => Teacher i -> Automaton _ i
lstar = learnAngluinRows
lstarCol = learnAngluin
nlstar = learnBollig 0 0

target :: _ => Automaton q i -> Teacher i
target = teacherWithTarget

targetNonDet :: _ => Int -> Automaton q i -> Teacher i
targetNonDet = teacherWithTargetNonDet
