{-# LANGUAGE RecordWildCards #-}
module NLStar where

import           Examples
import           Functions
import           ObservationTable
import           Teacher

import           NLambda

import           Debug.Trace
import           Data.List        (inits, tails)
import           Prelude          hiding (and, curry, filter, lookup, map, not,
                                   sum)

-- So at the moment we only allow sums of the form a + b
-- Of course we should approximate the powerset a bit better
-- But for the main example, we know this is enough!
-- I (Joshua) believe it is possible to give a finite-orbit
-- approximation, but the code will not be efficient ;-).
hackApproximate :: NominalType a => Set a -> Set (Set a)
hackApproximate set = empty `union` map singleton set `union` pairsWith (\x y -> singleton x `union` singleton y) set set

rowUnion :: NominalType i => Set (BRow i) -> BRow i
rowUnion set = Prelude.uncurry union . setTrueFalse . partition (\(_, f) -> f) $ map (\is -> (is, exists fromBool (mapFilter (\(is2, b) -> maybeIf (is `eq` is2) b) flatSet))) allIs
    where
        flatSet = sum set
        allIs = map fst flatSet
        setTrueFalse (trueSet, falseSet) = (map (setSecond True) trueSet, map (setSecond False) falseSet)
        setSecond a (x, _) = (x, a)

-- lifted row functions
rowP t = rowUnion . map (row t)
rowPa t set a = rowUnion . map (\s -> rowa t s a) $ set

-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
incompletenessNonDet :: NominalType i => State i -> Set [i]
incompletenessNonDet State{..} = filter (not . hasEqRow) ssa
    where
        sss = map (rowP t) . hackApproximate $ ss
        -- true if the sequence sa has an equivalent row in ss
        hasEqRow = contains sss . row t

inconsistencyNonDet :: NominalType i => State i -> Set ((Set [i], Set [i], i), Set [i])
inconsistencyNonDet State{..} =
    pairsWithFilter (
        \(s1, s2) a -> maybeIf (candidate1 s1 s2 a) ((s1, s2, a), discrepancy (rowPa t s1 a) (rowPa t s2 a))
    ) rowPairs aa
    where
        rowPairs = pairsWithFilter (\s1 s2 -> maybeIf (candidate0 s1 s2) (s1,s2)) (hackApproximate ss) (hackApproximate ss)
        candidate0 s1 s2 = s1 `neq` s2 /\ rowP t s1 `eq` rowP t s2
        candidate1 s1 s2 a = rowPa t s1 a `neq` rowPa t s2 a

-- This function will (recursively) make the table complete and consistent.
-- This is in the IO monad purely because I want some debugging information.
-- (Same holds for many other functions here)
makeCompleteConsistentNonDet :: LearnableAlphabet i => Teacher i -> State i -> State i
makeCompleteConsistentNonDet teacher state@State{..} =
    -- inc is the set of rows witnessing incompleteness, that is the sequences
    -- 's1 a' which do not have their equivalents of the form 's2'.
    let inc = incompletenessNonDet state in
    ite (isNotEmpty inc)
        (   -- If that set is non-empty, we should add new rows
            trace "Incomplete! Adding rows:" $
            -- These will be the new rows, ...
            let ds = inc in
            traceShow ds $
            let state2 = addRows teacher ds state in
            makeCompleteConsistentNonDet teacher state2
        )
        (   -- inc2 is the set of inconsistencies.
            let inc2 = inconsistencyNonDet state in
            ite (isNotEmpty inc2)
                (   -- If that set is non-empty, we should add new columns
                    trace "Inconsistent! Adding columns:" $
                    -- The extensions are in the second component
                    let de = sum $ map (\((s1,s2,a),es) -> map (a:) es) inc2 in
                    traceShow de $
                    let state2 = addColumns teacher de state in
                    makeCompleteConsistentNonDet teacher state2
                )
                (   -- If both sets are empty, the table is complete and
                    -- consistent, so we are done.
                    trace " => Complete + Consistent :D!" $
                    state
                )
        )

boolImplies :: Bool -> Bool -> Bool
boolImplies True False = False
boolImplies _ _ = True

sublang :: NominalType i => BRow i -> BRow i -> Formula
sublang r1 r2 = forAll fromBool $ pairsWithFilter (\(i1, f1) (i2, f2) -> maybeIf (i1 `eq` i2) (f1 `boolImplies` f2)) r1 r2

-- Given a C&C table, constructs an automaton. The states are given by 2^E (not
-- necessarily equivariant functions)
constructHypothesisNonDet :: NominalType i => State i -> Automaton (BRow i) i
constructHypothesisNonDet State{..} = automaton q a d i f
    where
        q = map (row t) ss
        a = aa
        d = triplesWithFilter (\s a s2 -> maybeIf (sublang (row t s2) (rowa t s a)) (row t s, a, row t s2)) ss aa ss
        i = singleton $ row t []
        f = mapFilter (\s -> maybeIf (toform $ apply t (s, [])) (row t s)) ss
        toform s = forAll id . map fromBool $ s

-- I am not quite sure whether this variant is due to Rivest & Schapire or Maler & Pnueli.
useCounterExampleNonDet :: LearnableAlphabet i => Teacher i -> State i -> Set [i] -> State i
useCounterExampleNonDet teacher state@State{..} ces =
    trace "Using ce:" $
    traceShow ces $
    let de = sum . map (fromList . tails) $ ces in
    trace " -> Adding columns:" $
    traceShow de $
    addColumns teacher de state

-- The main loop, which results in an automaton. Will stop if the hypothesis
-- exactly accepts the language we are learning.
loopNonDet :: LearnableAlphabet i => Teacher i -> State i -> Automaton (BRow i) i
loopNonDet teacher s =
    trace "##################" $
    trace "1. Making it complete and consistent" $
    let s2 = makeCompleteConsistentNonDet teacher s in
    trace "2. Constructing hypothesis" $
    let h = constructHypothesisNonDet s2 in
    traceShow h $
    trace "3. Equivalent? " $
    let eq = equivalent teacher h in
    traceShow eq $
    case eq of
        Nothing -> h
        Just ce -> do
            let s3 = useCounterExampleNonDet teacher s2 ce
            loopNonDet teacher s3

constructEmptyStateNonDet :: LearnableAlphabet i => Teacher i -> State i
constructEmptyStateNonDet teacher =
    let aa = Teacher.alphabet teacher in
    let ss = singleton [] in
    let ssa = pairsWith (\s a -> s ++ [a]) ss aa in
    let ee = singleton [] in
    let t = fillTable teacher (ss `union` ssa) ee in
    State{..}

learnNonDet :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnNonDet teacher = loopNonDet teacher s
    where s = constructEmptyStateNonDet teacher
