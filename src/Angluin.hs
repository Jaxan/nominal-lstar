{-# language RecordWildCards #-}
module Angluin where

import AbstractLStar
import ObservationTable
import Teacher

import Data.List (inits, tails)
import Debug.Trace
import NLambda
import Prelude (Bool (..), Maybe (..), id, show, ($), (++), (.))
import qualified Prelude hiding ()

justOne :: (Contextual a, NominalType a) => Set a -> Set a
justOne s = mapFilter id . orbit [] . element $ s

-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
closednessTest :: LearnableAlphabet i => State i -> TestResult i
closednessTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed (justOne defect) empty
    where
        sss = map (row t) ss                 -- all the rows
        hasEqRow = contains sss . row t      -- has equivalent upper row
        defect = filter (not . hasEqRow) ssa -- all rows without equivalent guy

-- We look for inconsistencies and return columns witnessing it
consistencyTestDirect :: LearnableAlphabet i => State i -> TestResult i
consistencyTestDirect State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty (justOne defect)
    where
        ssRows = map (\u -> (u, row t u)) ss
        candidates = pairsWithFilter (\(u1,r1) (u2,r2) -> maybeIf (u1 `neq` u2 /\ r1 `eq` r2) (u1, u2)) ssRows ssRows
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (tableAt t (u1 ++ [a]) v `diff` tableAt t (u2 ++ [a]) v) (a:v)) candidates aa ee
        diff a b = not (a `iff` b)


-- Given a C&C table, constructs an automaton. The states are given by 2^E (not
-- necessarily equivariant functions)
constructHypothesis :: LearnableAlphabet i => State i -> Automaton (BRow i) i
constructHypothesis State{..} = simplify $ automaton q a d i f
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
useCounterExampleMP :: LearnableAlphabet i => Teacher i -> State i -> Set [i] -> State i
useCounterExampleMP teacher state@State{..} ces =
    trace ("Using ce: " ++ show ces) $
    let de = sum . map (fromList . tails) $ ces in
    addColumns teacher de state

-- Putting the above together in a learning algorithm
makeCompleteAngluin :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteAngluin = makeCompleteWith [closednessTest, consistencyTestDirect]

-- Default: use counter examples in columns, which is slightly faster
learnAngluin :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnAngluin teacher = learn makeCompleteAngluin useCounterExampleMP constructHypothesis teacher initial
    where initial = constructEmptyState 0 0 teacher

-- The "classical" version, where counter examples are added as rows
learnAngluinRows :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnAngluinRows teacher = learn makeCompleteAngluin useCounterExampleAngluin constructHypothesis teacher initial
    where initial = constructEmptyState 0 0 teacher


-- Below are some variations of the above functions with different
-- performance characteristics.

-- Some coauthor's slower version
consistencyTest2 :: NominalType i => State i -> TestResult i
consistencyTest2 State{..} = case solve (isEmpty defect) of
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
        columns = sum $ map (\((_,_,a),es) -> map (a:) es) defect

-- Some coauthor's faster version
consistencyTest3 :: NominalType i => State i -> TestResult i
consistencyTest3 State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty columns
    where
        rowPairs = pairsWithFilter (\s1 s2 -> maybeIf (candidate0 s1 s2) (s1,s2)) ss ss
        candidate0 s1 s2 = s1 `neq` s2 /\ row t s1 `eq` row t s2
        candidate1 s1 s2 a = rowa t s1 a `neq` rowa t s2 a
        defect = pairsWithFilter (
                     \(s1, s2) a -> maybeIf (candidate1 s1 s2 a) ((s1, s2, a), discrepancy (rowa t s1 a) (rowa t s2 a))
                 ) rowPairs aa
        columns = sum $ map (\((_,_,a),es) -> map (a:) es) defect
