{-# language RecordWildCards #-}
module Bollig where

import AbstractLStar
import Angluin
import ObservationTable
import Teacher

import Debug.Trace
import NLambda
import Prelude (Bool (..), Int, Maybe (..), ($), (++), (.))

-- Comparing two graphs of a function is inefficient in NLambda,
-- because we do not have a map data structure. (So the only way
-- is by taking a product and filtering on equal inputs.)
-- So instead of considering a row as E -> 2, we simply take it
-- as a subset.
-- This does hinder generalisations to other nominal join semi-
-- lattices than the Booleans.
brow :: (NominalType i) => Table i Bool -> [i] -> Set [i]
brow t is = mapFilter (\((a,b),c) -> maybeIf (eq is a /\ fromBool c) b) t

rfsaClosednessTest :: LearnableAlphabet i => Set (Set [i]) -> State i -> TestResult i
rfsaClosednessTest primesUpp State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> trace "@@@ Unsolved Formula (rfsaClosednessTest) @@@" $
                  Failed defect empty
    where
        defect = filter (\ua -> brow t ua `neq` sum (filter (`isSubsetOf` brow t ua) primesUpp)) ssa

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
constructHypothesisBollig :: NominalType i => Set (Set [i]) -> State i -> Automaton (Set [i]) i
constructHypothesisBollig primesUpp State{..} = automaton q aa d i f
    where
        q = primesUpp
        i = filter (`isSubsetOf` brow t []) q
        f = filter (`contains` []) q
        d0 = triplesWithFilter (\s a s2 -> maybeIf (brow t s2 `isSubsetOf` brow t (s ++ [a])) (brow t s, a, brow t s2)) ss aa ss
        d = filter (\(q1, _, q2) -> q1 `member` q /\ q2 `member` q) d0

--makeCompleteBollig :: LearnableAlphabet i => TableCompletionHandler i
--makeCompleteBollig = makeCompleteWith [rfsaClosednessTest, rfsaConsistencyTest]

learnBollig :: LearnableAlphabet i => Int -> Int -> Teacher i -> Automaton (Set [i]) i
--learnBollig k n teacher = learn makeCompleteBollig useCounterExampleMP constructHypothesisBollig teacher initial
--    where initial = constructEmptyState k n teacher

learnBollig k n teacher = learnBolligLoop teacher (constructEmptyState k n teacher)

learnBolligLoop teacher s1@State{..} =
    let
        allRowsUpp = map (brow t) ss
        allRows = allRowsUpp `union` map (brow t) ssa
        primesUpp = filter (\r -> isNotEmpty r /\ r `neq` sum (filter (`isSubsetOf` r) (allRows \\ orbit [] r))) allRowsUpp

        -- No worry, these are computed lazily
        closednessRes = rfsaClosednessTest primesUpp s1
        consistencyRes = rfsaConsistencyTest s1
        h = constructHypothesisBollig primesUpp s1
    in
        trace "1. Making it rfsa closed" $
        case closednessRes of
            Failed newRows _ ->
                let state2 = simplify $ addRows teacher newRows s1 in
                learnBolligLoop teacher state2
            Succes ->
                trace "1. Making it rfsa consistent" $
                case consistencyRes of
                    Failed _ newColumns ->
                        let state2 = simplify $ addColumns teacher newColumns s1 in
                        learnBolligLoop teacher state2
                    Succes ->
                        traceShow h $
                        trace "3. Equivalent? " $
                        eqloop s1 h
                        where
                            eqloop s2 h = case equivalent teacher h of
                                            Nothing -> trace "Yes" h
                                            Just ces -> trace "No" $
                                                if isTrue . isEmpty $ realces h ces
                                                    then eqloop s2 h
                                                    else
                                                        let s3 = useCounterExampleMP teacher s2 ces in
                                                        learnBolligLoop teacher s3
                            realces h ces = NLambda.filter (\(ce, a) -> a `neq` accepts h ce) $ membership teacher ces

