{-# LANGUAGE RecordWildCards #-}
module Angluin where

import AbstractLStar
import ObservationTable
import Teacher

import Data.List (inits, tails)
import Debug.Trace
import NLambda
import qualified Prelude hiding ()
import Prelude (Bool(..), Maybe(..), id, ($), (.), (++), fst, show)

-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
closednessTest :: NominalType i => State i -> TestResult i
closednessTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    where
        sss = map (row t) ss                 -- all the rows
        hasEqRow = contains sss . row t      -- has equivalent upper row
        defect = filter (not . hasEqRow) ssa -- all rows without equivalent guy

-- We can determine its consistency with the following
consistencyTestJ :: NominalType i => State i -> TestResult i -- Set (([i], [i], i), Set [i])
consistencyTestJ State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty columns
    where
        -- true for equal rows, but unequal extensions
        -- we can safely skip equal sequences
        candidate s1 s2 a = s1 `neq` s2
                            /\ row t s1 `eq` row t s2
                            /\ rowa t s1 a `neq` rowa t s2 a
        defect = triplesWithFilter (
                     \s1 s2 a -> maybeIf (candidate s1 s2 a) ((s1, s2, a), discrepancy (rowa t s1 a) (rowa t s2 a))
                 ) ss ss aa
        columns = sum $ map (\((s1,s2,a),es) -> map (a:) es) defect

-- Bartek's faster version
consistencyTestB :: NominalType i => State i -> TestResult i -- Set (([i], [i], i), Set [i])
consistencyTestB State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty columns
    where
        rowPairs = pairsWithFilter (\s1 s2 -> maybeIf (candidate0 s1 s2) (s1,s2)) ss ss
        candidate0 s1 s2 = s1 `neq` s2 /\ row t s1 `eq` row t s2
        candidate1 s1 s2 a = rowa t s1 a `neq` rowa t s2 a
        defect = pairsWithFilter (
                     \(s1, s2) a -> maybeIf (candidate1 s1 s2 a) ((s1, s2, a), discrepancy (rowa t s1 a) (rowa t s2 a))
                 ) rowPairs aa
        columns = sum $ map (\((s1,s2,a),es) -> map (a:) es) defect

-- Given a C&C table, constructs an automaton. The states are given by 2^E (not
-- necessarily equivariant functions)
constructHypothesis :: NominalType i => State i -> Automaton (BRow i) i
constructHypothesis State{..} = automaton q a d i f
    where
        q = map (row t) ss
        a = aa
        d = pairsWith (\s a -> (row t s, a, rowa t s a)) ss aa
        i = singleton $ row t []
        f = mapFilter (\s -> maybeIf (toform $ apply t (s, [])) (row t s)) ss
        toform s = forAll id . map fromBool $ s

-- Extends the table with all prefixes of a set of counter examples.
useCounterExampleAngluin :: LearnableAlphabet i => Teacher i -> State i -> Set [i] -> State i
useCounterExampleAngluin teacher state@State{..} ces =
    trace ("Using ce: " ++ show ces) $
    let ds = sum . map (fromList . inits) $ ces in
    addRows teacher ds state

-- This is the variant by Maler and Pnueli
-- I used to think it waw Rivest and Schapire, but they add less columns
useCounterExampleMP :: LearnableAlphabet i => Teacher i -> State i -> Set [i] -> State i
useCounterExampleMP teacher state@State{..} ces =
    trace ("Using ce: " ++ show ces) $
    let de = sum . map (fromList . tails) $ ces in
    addColumns teacher de state

makeCompleteAngluin :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteAngluin = makeCompleteWith [closednessTest, consistencyTestB]

-- Default: use counter examples in columns, which is slightly faster
learnAngluin :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnAngluin teacher = learn makeCompleteAngluin useCounterExampleMP constructHypothesis teacher initial
    where initial = constructEmptyState teacher

-- The "classical" version, where counter examples are added as rows
learnAngluinRows :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnAngluinRows teacher = learn makeCompleteAngluin useCounterExampleAngluin constructHypothesis teacher initial
    where initial = constructEmptyState teacher
