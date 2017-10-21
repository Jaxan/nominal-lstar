{-# LANGUAGE RecordWildCards #-}
module AbstractLStar where

import ObservationTable
import Teacher

import Debug.Trace
import NLambda

type TableCompletionHandler i = Teacher i -> State i -> State i
type CounterExampleHandler i  = Teacher i -> State i -> Set [i] -> State i
type HypothesisConstruction i = State i -> Automaton (BRow i) i

data TestResult i
    = Succes                     -- test succeeded, no changes required
    | Failed (Set [i]) (Set [i]) -- test failed, change: add rows + columns

-- Simple loop which performs tests such as closedness and consistency
-- on the table and makes changes if needed. Will (hopefully) reach a
-- fixed point, i.e. a complete table.
makeCompleteWith :: LearnableAlphabet i
  => [State i -> TestResult i]
  -> Teacher i -> State i -> State i
makeCompleteWith tests teacher state0 = go tests state0
    where
        -- All tests succeeded, then the state is stable
        go [] state = state
        -- We still have tests to perform
        go (t:ts) state = case t state of
            -- If the test succeeded, we continue with the next one
            Succes -> go ts state
            -- Otherwise we add the changes
            Failed newRows newColumns ->
                let state2 = simplify $ addRows teacher newRows state in
                let state3 = simplify $ addColumns teacher newColumns state2 in
                -- restart the whole business
                makeCompleteWith tests teacher state3

-- Simple general learning loop: 1. make the table complete 2. construct
-- hypothesis 3. ask teacher. Repeat until done. If the teacher is adequate
-- termination implies correctness.
learn :: LearnableAlphabet i
  => TableCompletionHandler i
  -> CounterExampleHandler i
  -> HypothesisConstruction i
  -> Teacher i
  -> State i
  -> Automaton (BRow i) i
learn makeComplete handleCounterExample constructHypothesis teacher s =
    trace "##################" $
    trace "1. Making it complete and consistent" $
    let s2 = makeComplete teacher s in
    trace "2. Constructing hypothesis" $
    let h = constructHypothesis s2 in
    traceShow h $
    trace "3. Equivalent? " $
    eqloop s2 h
    where
        eqloop s2 h = case equivalent teacher h of
                          Nothing -> trace "Yes" $ h
                          Just ces -> trace "No" $
                              case isTrue . isEmpty $ realces h ces of
                                  True -> eqloop s2 h
                                  False ->
                                      let s3 = handleCounterExample teacher s2 ces in
                                      learn makeComplete handleCounterExample constructHypothesis teacher s3
        realces h ces = NLambda.filter (\(ce, a) -> a `neq` accepts h ce) $ membership teacher ces

-- Initial state is always the same in our case
constructEmptyState :: LearnableAlphabet i => Teacher i -> State i
constructEmptyState teacher =
    let aa = Teacher.alphabet teacher in
    let ss = singleton [] in
    let ssa = pairsWith (\s a -> s ++ [a]) ss aa in
    let ee = singleton [] in
    let t = fillTable teacher (ss `union` ssa) ee in
    State{..}
