{-# language FlexibleContexts #-}
{-# language PartialTypeSignatures #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Bollig where

import AbstractLStar
import qualified BooleanObservationTable as BOT
import ObservationTableClass
import qualified SimpleObservationTable as SOT
import Teacher

import Data.List (tails)
import Debug.Trace (trace, traceShow)
import NLambda hiding (alphabet)
import Prelude (Bool (..), Int, Maybe (..), Show (..), ($), (++), (.))

-- Comparing two graphs of a function is inefficient in NLambda,
-- because we do not have a map data structure. (So the only way
-- is by taking a product and filtering on equal inputs.)
-- So instead of considering a row as E -> 2, we simply take it
-- as a subset.
-- This does hinder generalisations to other nominal join semi-
-- lattices than the Booleans.

rfsaClosednessTest :: (Nominal i, _) => Set (Row table) -> table -> TestResult i
rfsaClosednessTest primesUpp t = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> trace "@@@ Unsolved Formula (rfsaClosednessTest) @@@" $
                  Failed defect empty
    where
        defect = filter (\ua -> row t ua `neq` sum (filter (`isSubsetOf` row t ua) primesUpp)) (rowsExt t)

rfsaConsistencyTest :: (Nominal i, _) => table -> TestResult i
rfsaConsistencyTest t = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty defect
    Nothing    -> trace "@@@ Unsolved Formula (rfsaConsistencyTest) @@@" $
                  Failed empty defect
    where
        candidates = pairsWithFilter (\u1 u2 -> maybeIf (row t u2 `isSubsetOf` row t u1) (u1, u2)) (rows t) (rows t)
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (not (tableAt2 (u1 ++ [a]) v) /\ tableAt2 (u2 ++ [a]) v) (a:v)) candidates (alph t) (cols t)
        tableAt2 s e = singleton True `eq` tableAt t s e

constructHypothesisBollig :: (Nominal i, _) => Set (Row table) -> table -> Automaton (Row table) i
constructHypothesisBollig primesUpp t = automaton q (alph t) d i f
    where
        q = primesUpp
        i = filter (`isSubsetOf` rowEps t) q
        f = filter (`contains` []) q
        -- TODO: compute indices of primesUpp only once
        d0 = triplesWithFilter (\s a bs2 -> maybeIf (bs2 `isSubsetOf` row t (s ++ [a])) (row t s, a, bs2)) (rows t) (alph t) q
        d = filter (\(q1, _, _) -> q1 `member` q) d0

-- Adds all suffixes as columns
-- TODO: do actual Rivest and Schapire
addCounterExample :: (Nominal i, _) => MQ i Bool -> Set [i] -> table -> table
addCounterExample mq ces t =
    let newColumns = sum . map (fromList . tails) $ ces
        newColumnsRed = newColumns \\ cols t
     in addColumns mq newColumnsRed t

learnBollig :: (Nominal i, _) => Int -> Int -> Teacher i -> Automaton _ i
learnBollig k n teacher = learnBolligLoop teacher (BOT.initialBTableSize (mqToBool teacher) (alphabet teacher) k n)

-- Slow version
learnBolligOld :: (Nominal i, _) => Int -> Int -> Teacher i -> Automaton _ i
learnBolligOld k n teacher = learnBolligLoop teacher (SOT.initialBTableSize (mqToBool teacher) (alphabet teacher) k n)

learnBolligLoop :: (Nominal i, _) => Teacher i -> table -> Automaton (Row table) i
learnBolligLoop teacher t =
    let
        -- These simplify's do speed up
        allRowsUpp = simplify $ map (row t) (rows t)
        allRows = simplify $ allRowsUpp `union` map (row t) (rowsExt t)
        primesUpp = simplify $ filter (\r -> isNotEmpty r /\ r `neq` sum (filter (`isSubsetOf` r) (allRows \\ orbit [] r))) allRowsUpp

        -- No worry, these are computed lazily
        closednessRes = rfsaClosednessTest primesUpp t
        consistencyRes = rfsaConsistencyTest t
        hyp = constructHypothesisBollig primesUpp t
    in
        trace "1. Making it rfsa closed" $
        case closednessRes of
            Failed newRows _ ->
                let state2 = addRows (mqToBool teacher) newRows t in
                trace ("newrows = " ++ show newRows) $
                learnBolligLoop teacher state2
            Succes ->
                trace "2. Making it rfsa consistent" $
                case consistencyRes of
                    Failed _ newColumns ->
                        let state2 = addColumns (mqToBool teacher) newColumns t in
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
                                                        trace ("Using ce: " ++ show ces) $
                                                        learnBolligLoop teacher s3
                            realces h ces = NLambda.filter (\(ce, a) -> a `neq` accepts h ce) $ membership teacher ces
