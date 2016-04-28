{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

import           Examples
import           Functions
import           ObservationTable
import           Teacher

import           NLambda

import           Data.List        (inits)
import           Prelude          hiding (and, curry, filter, lookup, map, not,
                                   sum, uncurry)


-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
incompleteness :: NominalType i => State i -> Set [i]
incompleteness State{..} = filter (not . hasEqRow) ssa
    where
        -- true if the sequence sa has an equivalent row in ss
        hasEqRow sa = exists (\s2 -> eq (row t sa) (row t s2)) ss

-- We can determine its consistency with the following
-- Returns equivalent rows (fst) with all inequivalent extensions (snd)
inconsistency :: NominalType i => State i -> Set (([i], [i], i), Set [i])
inconsistency State{..} =
    triplesWithFilter (
        \s1 s2 a -> maybeIf (candidate s1 s2 a) ((s1, s2, a), discrepancy (rowa t s1 a) (rowa t s2 a))
    ) ss ss aa
    where
        -- true for equal rows, but unequal extensions
        -- we can safely skip equal sequences
        candidate s1 s2 a = s1 `neq` s2
                            /\ row t s1 `eq` row t s2
                            /\ rowa t s1 a `neq` rowa t s2 a

-- This can be written for all monads. Unfortunately (a,) is also a monad and
-- this gives rise to overlapping instances, so I only do it for IO here.
-- Note that it is not really well defined, but it kinda works.
instance (Conditional a) => Conditional (IO a) where
    cond f a b = case solve f of
        Just True -> a
        Just False -> b
        Nothing -> fail "### Unresolved branch ###"
        -- NOTE: another implementation would be to evaluate both a and b
        -- and apply ite to their results. This however would runs both side
        -- effects of a and b.

-- This function will (recursively) make the table complete and consistent.
-- This is in the IO monad purely because I want some debugging information.
-- (Same holds for many other functions here)
makeCompleteConsistent :: (Show i, Contextual i, NominalType i, Teacher t i) => t -> State i -> IO (State i)
makeCompleteConsistent teacher state@State{..} = do
    putStrLn ""
    print state
    putStrLn ""
    -- inc is the set of rows witnessing incompleteness, that is the sequences
    -- 's1 a' which do not have their equivalents of the form 's2'.
    let inc = incompleteness state
    ite (isNotEmpty inc)
        (do
            -- If that set is non-empty, we should add new rows
            putStrLn "Incomplete!"
            -- These will be the new rows, ...
            let ds = inc
            putStr " -> Adding rows: "
            print ds
            let state2 = addRows teacher ds state
            makeCompleteConsistent teacher state2
        )
        (do
            -- inc2 is the set of inconsistencies.
            let inc2 = inconsistency state
            ite (isNotEmpty inc2)
                (do
                    -- If that set is non-empty, we should add new columns
                    putStr "Inconsistent! : "
                    print inc2
                    -- The extensions are in the second component
                    let de = sum $ map (\((s1,s2,a),es) -> map (a:) es) inc2
                    putStr " -> Adding columns: "
                    print de
                    let state2 = addColumns teacher de state
                    makeCompleteConsistent teacher state2
                )
                (do
                    -- If both sets are empty, the table is complete and
                    -- consistent, so we are done.
                    putStrLn " => Complete + Consistent :D!"
                    return state
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
useCounterExample :: (Show i, Contextual i, NominalType i, Teacher t i) => t -> State i -> Set [i] -> IO (State i)
useCounterExample teacher state@State{..} ces = do
    putStr "Using ce: "
    print ces
    let ds = sum . map (fromList . inits) $ ces
    putStr " -> Adding rows: "
    print ds
    let state2 = addRows teacher ds state
    return state2

-- The main loop, which results in an automaton. Will stop if the hypothesis
-- exactly accepts the language we are learning.
loop :: (Show i, Contextual i, NominalType i, Teacher t i) => t -> State i -> IO (Automaton (BRow i) i)
loop teacher s = do
    putStrLn "##################"
    putStrLn "1. Making it complete and consistent"
    s <- makeCompleteConsistent teacher s
    putStrLn "2. Constructing hypothesis"
    let h = constructHypothesis s
    print h
    putStr "3. Equivalent? "
    let eq = equivalent teacher h
    print eq
    case eq of
        Nothing -> return h
        Just ce -> do
            s <- useCounterExample teacher s ce
            loop teacher s

constructEmptyState :: (Contextual i, NominalType i, Teacher t i) => t -> State i
constructEmptyState teacher =
    let aa = Teacher.alphabet teacher in
    let ss = singleton [] in
    let ssa = pairsWith (\s a -> s ++ [a]) ss aa in
    let ee = singleton [] in
    let t = fillTable teacher (ss `union` ssa) ee in
    State{..}

learn :: (Show i, Contextual i, NominalType i, Teacher t i) => t -> IO (Automaton (BRow i) i)
learn teacher = do
    let s = constructEmptyState teacher
    loop teacher s

-- Initializes the table and runs the algorithm.
main :: IO ()
main = do
    h <- learn exampleTeacher
    putStrLn "Finished! Final hypothesis ="
    print h
