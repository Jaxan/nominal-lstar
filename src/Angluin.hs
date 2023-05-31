{-# language FlexibleContexts #-}
{-# language PartialTypeSignatures #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Angluin where

import AbstractLStar
import ObservationTableClass
import qualified BooleanObservationTable as OT
import Teacher

import Data.List (inits, tails)
import Debug.Trace
import NLambda hiding (alphabet)
import Prelude (Bool (..), Maybe (..), error, show, ($), (++), (.))


-- This returns all witnesses (of the form sa) for non-closedness
closednessTest :: (Nominal i, _) => table -> TestResult i
closednessTest t = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> let err = error "@@@ Unsolvable (closednessTest) @@@" in Failed err err
    where
        allRows = map (row t) (rows t)
        hasEqRow = contains allRows . row t
        defect = filter (not . hasEqRow) (rowsExt t)

-- We look for inconsistencies and return columns witnessing it
consistencyTestDirect :: (Nominal i, _) => table -> TestResult i
consistencyTestDirect t = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty defect
    Nothing    -> let err = error "@@@ Unsolvable (consistencyTestDirect) @@@" in Failed err err
    where
        ssRows = map (\u -> (u, row t u)) (rows t)
        candidates = pairsWithFilter (\(u1,r1) (u2,r2) -> maybeIf (u1 `neq` u2 /\ r1 `eq` r2) (u1, u2)) ssRows ssRows
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (tableAt t (u1 ++ [a]) v `neq` tableAt t (u2 ++ [a]) v) (a:v)) candidates (alph t) (cols t)

-- Given a C&C table, constructs an automaton. The states are given by 2^E (not
-- necessarily equivariant functions)
constructHypothesis :: (Nominal i, _) => table -> Automaton (Row table) i
constructHypothesis t = simplify $ automaton q (alph t) d i f
    where
        q = map (row t) (rows t)
        d = pairsWith (\s a -> (row t s, a, row t (s ++ [a]))) (rows t) (alph t)
        i = singleton (rowEps t)
        f = filter (`contains` []) q

-- Extends the table with all prefixes of a set of counter examples.
useCounterExampleAngluin :: (Nominal i, _) => Teacher i -> Set [i] -> table -> table
useCounterExampleAngluin teacher ces t =
    let newRows = sum . map (fromList . inits) $ ces
        newRowsRed = newRows \\ rows t
     in addRows (mqToBool teacher) newRowsRed t

-- This is the variant by Maler and Pnueli: Adds all suffixes as columns
useCounterExampleMP :: (Nominal i, _) => Teacher i -> Set [i] -> table -> table
useCounterExampleMP teacher ces t =
    let newColumns = sum . map (fromList . tails) $ ces
        newColumnsRed = newColumns \\ cols t
     in addColumns (mqToBool teacher) newColumnsRed t

-- Default: use counter examples in columns, which is slightly faster
learnAngluin :: (Nominal i, _) => Teacher i -> Automaton _ i
learnAngluin teacher = learnLoop useCounterExampleMP teacher (OT.initialBTable (mqToBool teacher) (alphabet teacher))

-- The "classical" version, where counter examples are added as rows
learnAngluinRows :: (Nominal i, _) => Teacher i -> Automaton _ i
learnAngluinRows teacher = learnLoop useCounterExampleAngluin teacher (OT.initialBTable (mqToBool teacher) (alphabet teacher))

learnLoop :: (Nominal i, ObservationTable table i Bool, _) => _ -> Teacher i -> table -> Automaton (Row table) i
learnLoop cexHandler teacher t =
    let
        -- No worry, these are computed lazily
        closednessRes = closednessTest t
        consistencyRes = consistencyTestDirect t
        hyp = constructHypothesis t
    in
        trace "1. Making it closed" $
        case closednessRes of
            Failed newRows _ ->
                let state2 = addRows (mqToBool teacher) newRows t in
                trace ("newrows = " ++ show newRows) $
                learnLoop cexHandler teacher state2
            Succes ->
                trace "2. Making it consistent" $
                case consistencyRes of
                    Failed _ newColumns ->
                        let state2 = addColumns (mqToBool teacher) newColumns t in
                        trace ("newcols = " ++ show newColumns) $
                        learnLoop cexHandler teacher state2
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
                                                        let s3 = cexHandler teacher ces s2 in
                                                        trace ("Using ce: " ++ show ces) $
                                                        learnLoop cexHandler teacher s3
                            realces h ces = NLambda.filter (\(ce, a) -> a `neq` accepts h ce) $ membership teacher ces
