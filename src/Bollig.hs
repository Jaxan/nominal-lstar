{-# LANGUAGE RecordWildCards #-}
module Bollig where

import AbstractLStar
import Angluin
import ObservationTable
import Teacher

import Data.List (tails)
import Debug.Trace
import NLambda
import qualified Prelude hiding ()
import Prelude (Bool(..), Maybe(..), ($), (.), (++), fst, show)

-- So at the moment we only allow sums of the form a + b and a + b + c
-- Of course we should approximate the powerset a bit better
-- But for the main examples, we know this is enough!
-- I (Joshua) believe it is possible to give a finite-orbit
-- approximation, but the code will not be efficient...
hackApproximate :: NominalType a => Set a -> Set (Set a)
hackApproximate set = empty
    `union` map singleton set
    `union` pairsWith (\x y -> singleton x `union` singleton y) set set
    `union` triplesWith (\x y z -> singleton x `union` singleton y `union` singleton z) set set set

rowUnion :: NominalType i => Set (BRow i) -> BRow i
rowUnion set = Prelude.uncurry union . setTrueFalse . partition (\(_, f) -> f) $ map (\is -> (is, exists fromBool (mapFilter (\(is2, b) -> maybeIf (is `eq` is2) b) flatSet))) allIs
    where
        flatSet = sum set
        allIs = map fst flatSet
        setTrueFalse (trueSet, falseSet) = (map (setSecond True) trueSet, map (setSecond False) falseSet)
        setSecond a (x, _) = (x, a)

primes :: NominalType a => (Set a -> a) -> Set a -> Set a
primes alg rows = filter (\r -> r `notMember` sumsWithout r) rows
    where
        sumsWithout r = map alg $ hackApproximate (rows \\ singleton r)

boolImplies :: Bool -> Bool -> Bool
boolImplies True False = False
boolImplies _ _ = True

sublang :: NominalType i => BRow i -> BRow i -> Formula
sublang r1 r2 = forAll fromBool $ pairsWithFilter (\(i1, f1) (i2, f2) -> maybeIf (i1 `eq` i2) (f1 `boolImplies` f2)) r1 r2

rfsaClosednessTest :: LearnableAlphabet i => State i -> TestResult i
rfsaClosednessTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> trace "@@@ Unsolved Formula (rfsaClosednessTest) @@@" $
                  Failed defect empty
    where
        defect = pairsWithFilter (\u a -> maybeIf (rowa t u a `member` primesDifference) (u ++ [a])) ss aa
        primesDifference = primes rowUnion (map (row t) $ ss `union` ssa) \\ map (row t) ss

rfsaConsistencyTest :: LearnableAlphabet i => State i -> TestResult i
rfsaConsistencyTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace ("Not consistent, defect = " ++ show defect) $ Failed empty defect
    Nothing    -> trace "@@@ Unsolved Formula (rfsaConsistencyTest) @@@" $
                  Failed empty defect
    where
        candidates = pairsWithFilter (\u1 u2 -> maybeIf (row t u2 `sublang` row t u1) (u1, u2)) ss ss
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (not (tableAt t (u1 ++ [a]) v) /\ tableAt t (u2++[a]) v) (a:v)) candidates aa ee

constructHypothesisBollig :: NominalType i => State i -> Automaton (BRow i) i
constructHypothesisBollig State{..} = automaton q a d i f
    where
        q = primes rowUnion (map (row t) ss)
        a = aa
        i = filter (\r -> r `sublang` row t []) q
        f = filter (\r -> singleton True `eq` mapFilter (\(i,b) -> maybeIf (i `eq` []) b) r) q
        d0 = triplesWithFilter (\s a s2 -> maybeIf (row t s2 `sublang` rowa t s a) (row t s, a, row t s2)) ss aa ss
        d = filter (\(q1,a,q2) -> q1 `member` q /\ q2 `member` q) d0

makeCompleteBollig :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteBollig = makeCompleteWith [rfsaClosednessTest, rfsaConsistencyTest]

learnBollig :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnBollig teacher = learn makeCompleteBollig useCounterExampleMP constructHypothesisBollig teacher initial
    where initial = constructEmptyState teacher
