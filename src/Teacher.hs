{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances         #-}

module Teacher where

import           NLambda hiding (alphabet, when)
import qualified NLambda (alphabet)

import Debug.Trace
-- Explicit Prelude, as NLambda has quite some clashes
import           Data.Function           (fix)
import           Data.List               (zip, (!!), reverse)
import           Data.Maybe              (Maybe (..))
import           Prelude                 (Bool (..), Int, Read, Show, error,
                                          length, return, ($), (++), (-), (<),
                                          (==), (.), (<=), (+), show, seq)
import qualified Prelude
import Control.Monad.Identity (Identity(..))
import Control.Monad (when)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Used in the IO teacher
import           System.Console.Haskeline
import           System.IO.Unsafe        (unsafePerformIO)
import           Text.Read               (readMaybe)

-- Abstract teacher type (inside the NLambda library, ideally one would like
-- an external interface, with Bool as output instead of Formula for instance)
data Teacher i = Teacher
    -- Given a sequence, check whether it is in the language
    -- Assumed to be equivariant
    { membership :: Set [i] -> Set ([i], Formula)
    -- Given a hypothesis, returns Nothing when equivalence or a (equivariant)
    -- set of counter examples. Needs to be quantified over q, because the
    -- learner may choose the type of the state space.
    , equivalent :: forall q. (Show q, NominalType q) => Automaton q i -> Maybe (Set [i])
    -- Returns the alphabet to the learner
    , alphabet   :: Set i
    }

-- In order to count membership queries, I had to have the set of inputs
-- instead of just mere inputs... It's a bit annoying, since it doesn't
-- look like Angluin like this... But this is the best I can do in NLambda
-- Here is a function to make it element-wise again:
foreachQuery :: NominalType i => ([i] -> Formula) -> Set[i] -> Set ([i], Formula)
foreachQuery f qs = map (\q -> (q, f q)) qs

-- We provide three ways to construct teachers:
-- 1. Fully automatic
-- 2. Fully interactive (via IO)
-- 3. Automatic membership, but interactive equivalence tests

-- 1. This is a fully automatic teacher, which has an internal automaton
-- Only works for DFAs for now, as those can be checked for equivalence
teacherWithTarget :: (NominalType i, NominalType q) => Automaton q i -> Teacher i
teacherWithTarget aut = Teacher
    { membership = foreachQuery $ automaticMembership aut
    , equivalent = automaticEquivalent bisim aut
    , alphabet   = automaticAlphabet aut
    }

-- 1b. This is a fully automatic teacher, which has an internal automaton
-- Might work for NFAs, not really tested
teacherWithTargetNonDet :: (Show i, Show q, NominalType i, NominalType q) => Int -> Automaton q i -> Teacher i
teacherWithTargetNonDet n aut = Teacher
    { membership = foreachQuery $ automaticMembership aut
    , equivalent = automaticEquivalent (bisimNonDet n) aut
    , alphabet   = automaticAlphabet aut
    }

-- 2. Will ask everything to someone reading the terminal
-- For the moment only Atom as input type
-- Note that parsing is very unforgiving, one mistake, and there is no way back
-- Atoms are referenced by Ints. When the user provides a counter example, we
-- consider the whole orbit generated by it.
teacherWithIO :: Teacher Atom
teacherWithIO = Teacher
    { membership = foreachQuery ioMembership
    , equivalent = ioEquivalent
    , alphabet   = atoms
    }

-- 3. A teacher uses a target for the mebership queries, but you for equivalence
-- Useful as long as you don't have an equivalence check, For example for G-NFAs
teacherWithTargetAndIO :: NominalType q => Automaton q Atom -> Teacher Atom
teacherWithTargetAndIO aut = Teacher
    { membership = foreachQuery $ automaticMembership aut
    , equivalent = ioEquivalent
    , alphabet   = atoms
    }

-- 4. A teacher with state (hacked, since the types don't allow for it)
-- Useful for debugging and so on, but *very very hacky*!
countingTeacher :: (Show i, Contextual i, NominalType i) => Teacher i -> Teacher i
countingTeacher delegate = Teacher
    { membership = \qs -> increaseMQ qs `seq` membership delegate qs
    , equivalent = \a -> increaseEQ a `seq` equivalent delegate a
    , alphabet   = alphabet delegate
    }
    where
        {-# NOINLINE increaseEQ #-}
        increaseEQ a = unsafePerformIO $ do
            i <- readIORef eqCounter
            let j = i + 1
            writeIORef eqCounter j
            return a
        {-# NOINLINE increaseMQ #-}
        increaseMQ q = unsafePerformIO $ do
            new <- newOrbitsInCache q
            l <- readIORef mqCounter
            let l2 = new : l
            writeIORef mqCounter l2
            return q
        {-# NOINLINE cache #-}
        cache = unsafePerformIO $ newIORef empty
        {-# NOINLINE newOrbitsInCache #-}
        newOrbitsInCache qs = do
            oldCache <- readIORef cache
            let newQs = simplify $ qs \\ oldCache
            writeIORef cache (simplify $ oldCache `union` qs)
            return $ setOrbitsMaxNumber newQs

-- HACK: Counts number of equivalence queries
eqCounter :: IORef Int
{-# NOINLINE eqCounter #-}
eqCounter = unsafePerformIO $ newIORef 0

-- HACK: Keeps track of membership queries with: # orbits per 'query'
mqCounter :: IORef [Int]
{-# NOINLINE mqCounter #-}
mqCounter = unsafePerformIO $ newIORef []

-- Implementations of above functions
automaticMembership aut input = accepts aut input
automaticEquivalent bisimlator aut hypo = case solve isEq of
        Nothing -> error "should be solved"
        Just True -> Nothing
        Just False -> Just bisimRes
        where
            bisimRes = bisimlator aut hypo
            isEq = isEmpty bisimRes
automaticAlphabet aut = NLambda.alphabet aut


instance Conditional a => Conditional (Identity a) where
    cond f x y = return (cond f (runIdentity x) (runIdentity y))

-- Checks bisimulation of initial states
-- I am not sure whether it does the right thing for non-det automata
-- returns some counter examples if not bisimilar
-- returns empty set iff bisimilar
bisim :: (NominalType i, NominalType q1, NominalType q2) => Automaton q1 i -> Automaton q2 i -> Set [i]
bisim aut1 aut2 = runIdentity $ go empty (pairsWith addEmptyWord (initialStates aut1) (initialStates aut2))
    where
        go rel todo = do
            -- if elements are already in R, we can skip them
            let todo2 = filter (\(_, x, y) -> (x, y) `notMember` rel) todo
            -- split into correct pairs and wrong pairs
            let (cont, ces) = partition (\(_, x, y) -> (x `member` (finalStates aut1)) <==> (y `member` (finalStates aut2))) todo2
            let aa = NLambda.alphabet aut1
            -- the good pairs should make one step
            let dtodo = sum (pairsWith (\(w, x, y) a -> pairsWith (\x2 y2 -> (a:w, x2, y2)) (d aut1 a x) (d aut2 a y)) cont aa)
            -- if there are wrong pairs
            ite (isNotEmpty ces)
                -- then return counter examples
                (return $ map getRevWord ces)
                -- else continue with good pairs
                (ite (isEmpty dtodo)
                    (return empty)
                    (go (rel `union` map stripWord cont) (dtodo))
                )
        d aut a x = mapFilter (\(s, l, t) -> maybeIf (s `eq` x /\ l `eq` a) t) (delta aut)
        stripWord (_, x, y) = (x, y)
        getRevWord (w, _, _) = reverse w
        addEmptyWord x y = ([], x, y)

-- Attempt at using a bisimlution up to to proof bisimulation between NFAs
-- Because why not? Inspired by the Hacking non-determinism paper
-- But they only consider finite sums (which is enough for finite sets)
-- Here I have to do a bit of trickery, which is hopefully correct.
-- I think it is correct, but not yet complete enough, we need more up-to.
bisimNonDet :: (Show i, Show q1, Show q2, NominalType i, NominalType q1, NominalType q2) => Int -> Automaton q1 i -> Automaton q2 i -> Set [i]
bisimNonDet n aut1 aut2 = runIdentity $ go empty (singleton ([], initialStates aut1, initialStates aut2))
    where
        go rel todo0 = do
            -- if elements are too long, we ignore them
            let todo0b = filter (\(w,_,_) -> fromBool (length w <= n)) todo0
            -- if elements are already in R, we can skip them
            let todo1 = filter (\(_, x, y) -> (x, y) `notMember` rel) todo0b
            -- now we are going to do a up-to thingy
            -- we look at all subsets x2 of x occuring in R (similarly for y)
            let xbar x = mapFilter (\(x2, _) -> maybeIf (x2 `isSubsetOf` x) x2) rel
            let ybar y = mapFilter (\(_, y2) -> maybeIf (y2 `isSubsetOf` y) y2) rel
            -- and then the sums are expressed by these formulea kind of
            let xform x y = x `eq` sum (xbar x) /\ forAll (\x2 -> exists (\y2 -> rel `contains` (x2, y2)) (ybar y)) (xbar x)
            let yform x y = y `eq` sum (ybar y) /\ forAll (\y2 -> exists (\x2 -> rel `contains` (x2, y2)) (xbar x)) (ybar y)
            let notSums x y = not (xform x y /\ yform x y)
            -- filter out things expressed as sums
            let todo2 = filter (\(_, x, y) -> notSums x y) todo1
            -- split into correct pairs and wrong pairs
            let (cont, ces) = partition (\(_, x, y) -> (x `intersect` (finalStates aut1)) <==> (y `intersect` (finalStates aut2))) todo2
            let aa = NLambda.alphabet aut1
            -- the good pairs should make one step
            let dtodo = pairsWith (\(w, x, y) a -> (a:w, sumMap (d aut1 a) x, sumMap (d aut2 a) y)) cont aa
            -- if there are wrong pairs
            --trace "go" $ traceShow rel $ traceShow todo0 $ traceShow todo1 $ traceShow todo2 $ traceShow cont $
            ite (isNotEmpty ces)
                -- then return counter examples
                (return $ map getRevWord ces)
                -- else continue with good pairs
                (ite (isEmpty dtodo)
                    (return empty)
                    (go (rel `union` map stripWord cont) (dtodo))
                )
        d aut a x = mapFilter (\(s, l, t) -> maybeIf (s `eq` x /\ l `eq` a) t) (delta aut)
        stripWord (_, x, y) = (x, y)
        getRevWord (w, _, _) = reverse w
        addEmptyWord x y = ([], x, y)
        sumMap f = sum . (map f)

ioMembership :: (Show i, NominalType i) => [i] -> Formula
ioMembership input = unsafePerformIO $ do
    let supp = leastSupport input
    Prelude.putStrLn "\n# Is the following word accepted?"
    Prelude.putStr "# "
    Prelude.print input
    Prelude.putStrLn "# You can answer with a formula (EQ, NEQ, AND, OR, T, F)"
    Prelude.putStrLn "# You may use the following atoms:"
    Prelude.putStr "# "
    Prelude.print $ zip supp [0..]
    answer <- runInputT defaultSettings loop
    return $ interpret supp answer
    where
        loop = do
            x <- getInputLine "> "
            case x of
                Nothing -> error "Quit"
                Just str -> do
                    case readMaybe str :: Maybe Form of
                        Nothing -> do
                            outputStrLn $ "Unable to parse " ++ str ++ " :: Form"
                            loop
                        Just f -> return f

ioEquivalent :: (Show q, NominalType q) => Automaton q Atom -> Maybe (Set [Atom])
ioEquivalent hypothesis = unsafePerformIO $ do
    Prelude.putStrLn "\n# Is the following automaton correct?"
    Prelude.putStr "# "
    Prelude.print hypothesis
    Prelude.putStrLn "# Nothing for Yes, Just [...] for a counter example"
    answer <- runInputT defaultSettings loop
    case answer of
        Nothing -> return Nothing
        Just input -> do
            -- create sequences of same length
            let n = length input
            let sequence = replicateAtoms n
            -- whenever two are identiacl in input, we will use eq, if not neq
            let op i j = if (input !! i) == (input !! j) then eq else neq
            -- copy the relations from input to sequence
            let rels s = and [op i j (s !! i) (s !! j) | i <- [0..n - 1], j <- [0..n - 1], i < j]
            let fseq = filter rels sequence
            return $ Just fseq
    where
        loop = do
            x <- getInputLine "> "
            case x of
                Nothing -> error "Quit"
                Just str -> do
                    case readMaybe str :: Maybe (Maybe [Prelude.String]) of
                        Nothing -> do
                            outputStrLn $ "Unable to parse " ++ str ++ " :: Maybe [String]"
                            loop
                        Just f -> return f

-- Data structure for reading formulas (with the derived Read instance)
data Form
    = EQ Int Int
    | NEQ Int Int
    | AND Form Form
    | OR Form Form
    | T
    | F
    deriving (Read)

interpret :: [Atom] -> Form -> Formula
interpret support (EQ i j) = eq (support !! i) (support !! j)
interpret support (NEQ i j) = neq (support !! i) (support !! j)
interpret support (AND f1 f2) = interpret support f1 /\ interpret support f2
interpret support (OR f1 f2) = interpret support f1 \/ interpret support f2
interpret _ T = true
interpret _ F = false
