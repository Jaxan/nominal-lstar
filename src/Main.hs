{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

import           Examples
import           Teacher

import           NLambda

import           Data.List        (inits)
import           Data.Maybe       (fromJust)
import           Prelude          hiding (and, curry, filter, lookup, map, not,
                                   sum, uncurry)

import           GHC.Generics     (Generic)

-- We represent functions as their graphs
type Fun a b = Set (a, b)

-- Basic manipulations on functions
-- Note that this returns a set, rather than an element
-- because we cannot extract a value from a singleton set
apply :: (NominalType a, NominalType b) => Fun a b -> a -> Set b
apply f a1 = mapFilter (\(a2, b) -> maybeIf (eq a1 a2) b) f

-- AxB -> c is adjoint to A -> C^B
-- curry and uncurry witnesses both ways of the adjunction
curry :: (NominalType a, NominalType b, NominalType c) => Fun (a, b) c -> Fun a (Fun b c)
curry f = map (\a1 -> (a1, mapFilter (\((a2,b),c) -> maybeIf (eq a1 a2) (b,c)) f)) as
    where as = map (\((a,b),c) -> a) f

uncurry :: (NominalType a, NominalType b, NominalType c) => Fun a (Fun b c) -> Fun (a, b) c
uncurry f = sum $ map (\(a,s) -> map (\(b,c) -> ((a, b), c)) s) f

-- Returns the subset (of the domain) which exhibits
-- different return values for the two functions
-- I am not sure about its correctness...
discrepancy :: (NominalType a, NominalType b) => Fun a b -> Fun a b -> Set a
discrepancy f1 f2 =
    pairsWithFilter (
        \(a1,b1) (a2,b2) -> maybeIf (eq a1 a2 /\ neq b1 b2) a1
    ) f1 f2


-- An observation table is a function S x E -> O
-- (Also includes SA x E -> O)
type Table i o = Fun ([i], [i]) o

-- `row is` denotes the data of a single row
-- that is, the function E -> O
row :: (NominalType i, NominalType o) => Table i o -> [i] -> Fun [i] o
row t is = sum (apply (curry t) is)

-- `rowa is a` is the row for the one letter extensions
rowa :: (NominalType i, NominalType o) => Table i o -> [i] -> i -> Fun [i] o
rowa t is a = row t (is ++ [a])

-- Our context
type Output = Bool

-- fills part of the table. First parameter is the rows (with extension),
-- second is columns. Although the teacher provides us formulas instead of
-- booleans, we can partition the answers to obtain actual booleans.
fillTable :: (Contextual i, NominalType i, Teacher t i) => t -> Set [i] -> Set [i] -> Table i Output
fillTable teacher sssa ee = sum2 . map2 (map slv) . map2 simplify . partition (\(s, e, f) -> f) $ base
    where
        base = pairsWith (\s e -> (s, e, membership teacher (s++e))) sssa ee
        map2 f (a, b) = (f a, f b)
        slv (a,b,f) = ((a,b), Data.Maybe.fromJust . solve $ f)
        sum2 (a,b) = a `union` b


-- Data structure representing the state of the learning algorithm (NOT a
-- state in the automaton)
data State i = State
    { t   :: Table i Output -- the table
    , ss  :: Set [i]        -- state sequences
    , ssa :: Set [i]        -- their one letter extensions
    , ee  :: Set [i]        -- suffixes
    , aa  :: Set i          -- alphabet (remains constant)
    }
    deriving (Show, Ord, Eq, Generic)

instance NominalType i => BareNominalType (State i)

instance NominalType i => Conditional (State i) where
    cond f s1 s2 = fromTup (cond f (toTup s1) (toTup s2)) where
        toTup State{..} = (t,ss,ssa,ee,aa)
        fromTup (t,ss,ssa,ee,aa) = State{..}

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
        candidate s1 s2 a = row t s1 `eq` row t s2
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
            -- ... and their extensions
            let dsa = pairsWith (\s a -> s ++ [a]) ds aa
            -- For the new rows, we fill the table
            -- note that `ds ee` is already filled
            let dt = fillTable teacher dsa ee
            putStr " -> delta table: "
            print dt
            -- And we repeat
            let state2 = state {t = t `union` dt, ss = ss `union` ds, ssa = ssa `union` dsa}
            makeCompleteConsistent teacher state2
        )
        (do
            -- inc2 is the set of inconsistencies.
            let inc2 = inconsistency state
            ite (isNotEmpty inc2)
                (do
                    -- If that set is non-empty, we should add new columns
                    putStrLn "Inconsistent!"
                    -- The extensions are in the second component
                    let de = sum $ map (\((s1,s2,a),es) -> map (a:) es) inc2
                    putStr " -> Adding columns: "
                    print de
                    -- Fill that part of the table
                    let dt = fillTable teacher (ss `union` ssa) de
                    putStr " -> delta table: "
                    print dt
                    -- And we continue
                    let state2 = state {t = t `union` dt, ee = ee `union` de}
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
constructHypothesis :: NominalType i => State i -> Automaton (Fun [i] Output) i
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
    -- as above, adding rows
    let dsa = pairsWith (\s a -> s ++ [a]) ds aa
    let dt = fillTable teacher dsa ee
    let state2 = state {t = t `union` dt, ss = ss `union` ds, ssa = ssa `union` dsa}
    return state2

-- The main loop, which results in an automaton. Will stop if the hypothesis
-- exactly accepts the language we are learning.
loop :: (Show i, Contextual i, NominalType i, Teacher t i) => t -> State i -> IO (Automaton (Fun [i] Output) i)
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

learn :: (Show i, Contextual i, NominalType i, Teacher t i) => t -> IO (Automaton (Fun [i] Output) i)
learn teacher = do
    let s = constructEmptyState teacher
    loop teacher s

-- Initializes the table and runs the algorithm.
main :: IO ()
main = do
    h <- learn exampleTeacher
    putStrLn "Finished! Final hypothesis ="
    print h
