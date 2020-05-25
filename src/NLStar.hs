{-# LANGUAGE RecordWildCards #-}
module NLStar where

import           AbstractLStar
import           Angluin
import           Bollig
import           ObservationTable
import           Teacher

import           NLambda

import           Debug.Trace
import           Data.List        (inits, tails)
import           Prelude          hiding (and, curry, filter, lookup, map, not,
                                   sum)

{- This is not NL* from the Bollig et al paper. This is a very naive
   approximation. You see, the consistency in their paper is quite weak,
   and in fact does not determine a well defined automaton (the constructed
   automaton does not even agree with the table of observations). They fix
   it by adding counter examples as columns instead of rows. This way the
   teacher will point out inconsistencies and columns are added then.

   Here, I propose an algorithm which does not check consistency at all!
   Sounds a bit crazy, but the teacher kind of takes care of that. Of course
   I do not know whether this will terminate. But it's nice to experiment with.
   Also I do not 'minimize' the NFA by taking only prime rows. Saves a lot of
   checking but makes the result not minimal (whatever that would mean). It
   is quite fast, however ;-).

   THIS IS NOT USED IN THE PAPER.
-}

-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
nonDetClosednessTest :: NominalType i => State i -> TestResult i
nonDetClosednessTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    where
        allRows = map (row t) ss
        hasSum r = r `eq` rowUnion (sublangs r allRows)
        defect = filter (not . hasSum . row t) ssa

constructHypothesisNonDet :: NominalType i => State i -> Automaton (BRow i) i
constructHypothesisNonDet State{..} = automaton q a d i f
    where
        q = map (row t) ss
        a = aa
        d = triplesWithFilter (\s a s2 -> maybeIf (row t s2 `sublang` rowa t s a) (row t s, a, row t s2)) ss aa ss
        i = singleton $ row t []
        f = mapFilter (\s -> maybeIf (tableAt t s []) (row t s)) ss

makeCompleteNonDet :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteNonDet = makeCompleteWith [nonDetClosednessTest]

-- Default: use counter examples in columns, which is slightly faster
learnNonDet :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnNonDet teacher = learn makeCompleteNonDet useCounterExampleMP constructHypothesisNonDet teacher initial
    where initial = constructEmptyState 0 0 teacher
