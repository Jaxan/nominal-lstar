{-# language RecordWildCards #-}
module Bollig where

import AbstractLStar
import Angluin
import ObservationTable
import Teacher

import Debug.Trace
import NLambda
import Prelude (Bool (..), Int, Maybe (..), fst, snd, ($), (++), (.), (<=))
import qualified Prelude hiding ()

-- Comparing two graphs of a function is inefficient in NLambda,
-- because we do not have a map data structure. (So the only way
-- is by taking a product and filtering on equal inputs.)
-- So instead of considering a row as E -> 2, we simply take it
-- as a subset.
-- This does hinder generalisations to other nominal join semi-
-- lattices than the Booleans.
brow :: (NominalType i) => Table i Bool -> [i] -> Set [i]
brow t is = mapFilter (\((a,b),c) -> maybeIf (eq is a /\ fromBool c) b) t

rfsaClosednessTest3 :: LearnableAlphabet i => State i -> TestResult i
rfsaClosednessTest3 State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> trace "@@@ Unsolved Formula (rfsaClosednessTest) @@@" $
                  Failed defect empty
    where
        defect = filter (\ua -> brow t ua `neq` sum (filter (`isSubsetOf` brow t ua) primesUpp)) ssa
        primesUpp = filter (\r -> isNotEmpty r /\ r `neq` sum (filter (`isSubsetOf` r) (allRows \\ orbit [] r))) allRowsUpp
        allRowsUpp = map (brow t) ss
        allRows = allRowsUpp `union` map (brow t) ssa

rfsaConsistencyTest :: LearnableAlphabet i => State i -> TestResult i
rfsaConsistencyTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty defect
    Nothing    -> trace "@@@ Unsolved Formula (rfsaConsistencyTest) @@@" $
                  Failed empty defect
    where
        candidates = pairsWithFilter (\u1 u2 -> maybeIf (brow t u2 `isSubsetOf` brow t u1) (u1, u2)) ss ss
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (not (tableAt t (u1 ++ [a]) v) /\ tableAt t (u2 ++ [a]) v) (a:v)) candidates aa ee

-- Note that we do not have the same type of states as the angluin algorithm.
-- We have Set [i] instead of BRow i. (However, They are isomorphic.)
constructHypothesisBollig :: NominalType i => State i -> Automaton (Set [i]) i
constructHypothesisBollig State{..} = automaton q a d i f
    where
        q = primesUpp
        a = aa
        i = filter (`isSubsetOf` brow t []) q
        f = filter (`contains` []) q
        d0 = triplesWithFilter (\s a s2 -> maybeIf (brow t s2 `isSubsetOf` brow t (s ++ [a])) (brow t s, a, brow t s2)) ss aa ss
        d = filter (\(q1, _, q2) -> q1 `member` q /\ q2 `member` q) d0
        primesUpp = filter (\r -> isNotEmpty r /\ r `neq` sum (filter (`isSubsetOf` r) (allRows \\ orbit [] r))) allRowsUpp
        allRowsUpp = map (brow t) ss
        allRows = allRowsUpp `union` map (brow t) ssa

makeCompleteBollig :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteBollig = makeCompleteWith [rfsaClosednessTest3, rfsaConsistencyTest]

learnBollig :: LearnableAlphabet i => Int -> Int -> Teacher i -> Automaton (Set [i]) i
learnBollig k n teacher = learn makeCompleteBollig useCounterExampleMP constructHypothesisBollig teacher initial
    where initial = constructEmptyState k n teacher
