{-# language PartialTypeSignatures #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Bollig where

import AbstractLStar
import SimpleObservationTable
import Teacher

import Data.List (tails)
import Debug.Trace
import NLambda hiding (alphabet)
import Prelude (Bool (..), Int, Maybe (..), Show (..), snd, ($), (++), (.))

-- Comparing two graphs of a function is inefficient in NLambda,
-- because we do not have a map data structure. (So the only way
-- is by taking a product and filtering on equal inputs.)
-- So instead of considering a row as E -> 2, we simply take it
-- as a subset.
-- This does hinder generalisations to other nominal join semi-
-- lattices than the Booleans.

-- The teacher interface is slightly inconvenient
-- But this is for a good reason. The type [i] -> o
-- doesn't work well in nlambda
mqToBool :: (NominalType i, Contextual i) => Teacher i -> MQ i Bool
mqToBool teacher words = simplify answer
    where
        realQ = membership teacher words
        (inw, outw) = partition snd realQ
        answer = map (setB True) inw `union` map (setB False) outw
        setB b (w, _) = (w, b)

tableAt :: NominalType i => BTable i -> [i] -> [i] -> Formula
tableAt t s e = singleton True `eq` mapFilter (\(i, o) -> maybeIf ((s ++ e) `eq` i) o) (content t)

rfsaClosednessTest :: NominalType i => Set (BRow i) -> BTable i -> TestResult i
rfsaClosednessTest primesUpp t@Table{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> trace "@@@ Unsolved Formula (rfsaClosednessTest) @@@" $
                  Failed defect empty
    where
        defect = filter (\ua -> brow t ua `neq` sum (filter (`isSubsetOf` brow t ua) primesUpp)) (rowsExt t)

rfsaConsistencyTest :: NominalType i => BTable i -> TestResult i
rfsaConsistencyTest t@Table{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty defect
    Nothing    -> trace "@@@ Unsolved Formula (rfsaConsistencyTest) @@@" $
                  Failed empty defect
    where
        candidates = pairsWithFilter (\u1 u2 -> maybeIf (brow t u2 `isSubsetOf` brow t u1) (u1, u2)) rows rows
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (not (tableAt t (u1 ++ [a]) v) /\ tableAt t (u2 ++ [a]) v) (a:v)) candidates alph columns

constructHypothesisBollig :: NominalType i => Set (BRow i) -> BTable i -> Automaton (BRow i) i
constructHypothesisBollig primesUpp t@Table{..} = automaton q alph d i f
    where
        q = primesUpp
        i = filter (`isSubsetOf` brow t []) q
        f = filter (`contains` []) q
        -- TODO: compute indices of primesUpp only once
        d0 = triplesWithFilter (\s a s2 -> maybeIf (brow t s2 `isSubsetOf` brow t (s ++ [a])) (brow t s, a, brow t s2)) rows alph rows
        d = filter (\(q1, _, q2) -> q1 `member` q /\ q2 `member` q) d0

-- Adds all suffixes as columns
-- TODO: do actual Rivest and Schapire
addCounterExample :: (NominalType i, _) => MQ i Bool -> Set [i] -> BTable i -> BTable i
addCounterExample mq ces t@Table{..} =
    trace ("Using ce: " ++ show ces) $
    let newColumns = sum . map (fromList . tails) $ ces
        newColumnsRed = newColumns \\ columns
     in addColumns mq newColumnsRed t

learnBollig :: (NominalType i, Contextual i, _) => Int -> Int -> Teacher i -> Automaton (BRow i) i
learnBollig k n teacher = learnBolligLoop teacher (initialTableSize (mqToBool teacher) (alphabet teacher) k n)

learnBolligLoop :: _ => Teacher i -> BTable i -> Automaton (BRow i) i
learnBolligLoop teacher t@Table{..} =
    let
        allRowsUpp = map (brow t) rows
        allRows = allRowsUpp `union` map (brow t) (rowsExt t)
        primesUpp = filter (\r -> isNotEmpty r /\ r `neq` sum (filter (`isSubsetOf` r) (allRows \\ orbit [] r))) allRowsUpp

        -- No worry, these are computed lazily
        closednessRes = rfsaClosednessTest primesUpp t
        consistencyRes = rfsaConsistencyTest t
        hyp = constructHypothesisBollig primesUpp t
    in
        trace "1. Making it rfsa closed" $
        case closednessRes of
            Failed newRows _ ->
                let state2 = simplify $ addRows (mqToBool teacher) newRows t in
                trace ("newrows = " ++ show newRows) $
                learnBolligLoop teacher state2
            Succes ->
                trace "2. Making it rfsa consistent" $
                case consistencyRes of
                    Failed _ newColumns ->
                        let state2 = simplify $ addColumns (mqToBool teacher) newColumns t in
                        trace ("newcols = " ++ show newColumns) $
                        learnBolligLoop teacher state2
                    Succes ->
                        traceShow hyp $
                        trace "3. Equivalent? " $
                        eqloop t hyp
                        where
                            eqloop s2 h = case equivalent teacher h of
                                            Nothing -> trace "Yes" h
                                            Just ces -> trace "No" $
                                                if isTrue . isEmpty $ realces h ces
                                                    then eqloop s2 h
                                                    else
                                                        let s3 = addCounterExample (mqToBool teacher) ces s2 in
                                                        learnBolligLoop teacher s3
                            realces h ces = NLambda.filter (\(ce, a) -> a `neq` accepts h ce) $ membership teacher ces
