{-# LANGUAGE RecordWildCards   #-}

import           Bollig
import           Examples
import           Functions
import           ObservationTable
import           Teacher
import           NLStar

import           NLambda

import Control.DeepSeq
import           Data.List        (inits, tails)
import           Debug.Trace
import           Prelude          hiding (and, curry, filter, lookup, map, not,
                                   sum, uncurry)


-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
incompleteness :: NominalType i => State i -> Set [i]
incompleteness State{..} = filter (not . hasEqRow) ssa
    where
        sss = map (row t) ss
        -- true if the sequence sa has an equivalent row in ss
        hasEqRow = contains sss . row t

-- We can determine its consistency with the following
-- Returns equivalent rows (fst) with all inequivalent extensions (snd)
inconsistencyJoshua :: NominalType i => State i -> Set (([i], [i], i), Set [i])
inconsistencyJoshua State{..} =
    triplesWithFilter (
        \s1 s2 a -> maybeIf (candidate s1 s2 a) ((s1, s2, a), discrepancy (rowa t s1 a) (rowa t s2 a))
    ) ss ss aa
    where
        -- true for equal rows, but unequal extensions
        -- we can safely skip equal sequences
        candidate s1 s2 a = s1 `neq` s2
                            /\ row t s1 `eq` row t s2
                            /\ rowa t s1 a `neq` rowa t s2 a

inconsistencyBartek :: NominalType i => State i -> Set (([i], [i], i), Set [i])
inconsistencyBartek State{..} =
    pairsWithFilter (
        \(s1, s2) a -> maybeIf (candidate1 s1 s2 a) ((s1, s2, a), discrepancy (rowa t s1 a) (rowa t s2 a))
    ) rowPairs aa
    where
        rowPairs = pairsWithFilter (\s1 s2 -> maybeIf (candidate0 s1 s2) (s1,s2)) ss ss
        candidate0 s1 s2 = s1 `neq` s2 /\ row t s1 `eq` row t s2
        candidate1 s1 s2 a = rowa t s1 a `neq` rowa t s2 a

inconsistency :: NominalType i => State i -> Set (([i], [i], i), Set [i])
inconsistency = inconsistencyBartek

-- This function will (recursively) make the table complete and consistent.
-- This is in the IO monad purely because I want some debugging information.
-- (Same holds for many other functions here)
makeCompleteConsistent :: LearnableAlphabet i => Teacher i -> State i -> State i
makeCompleteConsistent teacher state@State{..} =
    -- inc is the set of rows witnessing incompleteness, that is the sequences
    -- 's1 a' which do not have their equivalents of the form 's2'.
    let inc = incompleteness state in
    ite (isNotEmpty inc)
        (   -- If that set is non-empty, we should add new rows
            trace "Incomplete! Adding rows:" $
            -- These will be the new rows, ...
            let ds = inc in
            traceShow ds $
            let state2 = addRows teacher ds state in
            makeCompleteConsistent teacher state2
        )
        (   -- inc2 is the set of inconsistencies.
            let inc2 = inconsistency state in
            ite (isNotEmpty inc2)
                (   -- If that set is non-empty, we should add new columns
                    trace "Inconsistent! Adding columns:" $
                    -- The extensions are in the second component
                    let de = sum $ map (\((s1,s2,a),es) -> map (a:) es) inc2 in
                    traceShow de $
                    let state2 = addColumns teacher de state in
                    makeCompleteConsistent teacher state2
                )
                (   -- If both sets are empty, the table is complete and
                    -- consistent, so we are done.
                    trace " => Complete + Consistent :D!" $
                    state
                )
        )

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
    trace "Using ce:" $
    traceShow ces $
    let ds = sum . map (fromList . inits) $ ces in
    trace " -> Adding rows:" $
    traceShow ds $
    addRows teacher ds state

-- I am not quite sure whether this variant is due to Rivest & Schapire or Maler & Pnueli.
useCounterExampleRS :: LearnableAlphabet i => Teacher i -> State i -> Set [i] -> State i
useCounterExampleRS teacher state@State{..} ces =
    trace "Using ce:" $
    traceShow ces $
    let de = sum . map (fromList . tails) $ ces in
    trace " -> Adding columns:" $
    traceShow de $
    addColumns teacher de state

useCounterExample :: LearnableAlphabet i => Teacher i -> State i -> Set [i] -> State i
useCounterExample = useCounterExampleRS

-- The main loop, which results in an automaton. Will stop if the hypothesis
-- exactly accepts the language we are learning.
loop :: LearnableAlphabet i => Teacher i -> State i -> Automaton (BRow i) i
loop teacher s =
    -- I put a deepseq here in order to let all traces be evaluated
    -- in a decent order. Also it will be used anyways.
    deepseq s $
    trace "##################" $
    trace "1. Making it complete and consistent" $
    let s2 = makeCompleteConsistent teacher s in
    trace "2. Constructing hypothesis" $
    let h = constructHypothesis s2 in
    traceShow h $
    trace "3. Equivalent? " $
    let eq = equivalent teacher h in
    traceShow eq $
    case eq of
        Nothing -> h
        Just ce -> do
            let s3 = useCounterExample teacher s2 ce
            loop teacher s3

constructEmptyState :: LearnableAlphabet i => Teacher i -> State i
constructEmptyState teacher =
    let aa = Teacher.alphabet teacher in
    let ss = singleton [] in
    let ssa = pairsWith (\s a -> s ++ [a]) ss aa in
    let ee = singleton [] in
    let t = fillTable teacher (ss `union` ssa) ee in
    State{..}

learn :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learn teacher = loop teacher s
    where s = constructEmptyState teacher

-- Initializes the table and runs the algorithm.
main :: IO ()
main = do
    let h = learn (teacherWithTarget (Examples.fifoExample 3))
    putStrLn "Finished! Final hypothesis ="
    print h
