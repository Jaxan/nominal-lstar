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

rowUnion :: NominalType i => Set (BRow i) -> BRow i
rowUnion set = Prelude.uncurry union . setTrueFalse . partition (\(_, f) -> f) $ map (\is -> (is, exists fromBool (mapFilter (\(is2, b) -> maybeIf (is `eq` is2) b) flatSet))) allIs
    where
        flatSet = sum set
        allIs = map fst flatSet
        setTrueFalse (trueSet, falseSet) = (map (setSecond True) trueSet, map (setSecond False) falseSet)
        setSecond a (x, _) = (x, a)

boolImplies :: Bool -> Bool -> Bool
boolImplies True False = False
boolImplies _ _        = True

sublang :: NominalType i => BRow i -> BRow i -> Formula
sublang r1 r2 = forAll fromBool $ pairsWithFilter (\(i1, f1) (i2, f2) -> maybeIf (i1 `eq` i2) (f1 `boolImplies` f2)) r1 r2

sublangs :: NominalType i => BRow i -> Set (BRow i) -> Set (BRow i)
sublangs r set = filter (\r2 -> r2 `sublang` r) set

rfsaClosednessTest2 :: LearnableAlphabet i => State i -> TestResult i
rfsaClosednessTest2 State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    Nothing    -> trace "@@@ Unsolved Formula (rfsaClosednessTest) @@@" $
                  Failed defect empty
    where
        defect = pairsWithFilter (\u a -> maybeIf (rowa t u a `neq` rowUnion (sublangs (rowa t u a) primesUpp)) (u ++ [a])) ss aa
        primesUpp = filter (\r -> r `neq` rowUnion (sublangs r (allRows \\ orbit [] r))) allRowsUpp
        allRowsUpp = map (row t) ss
        allRows = allRowsUpp `union` map (row t) ssa

rfsaConsistencyTest :: LearnableAlphabet i => State i -> TestResult i
rfsaConsistencyTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty defect
    Nothing    -> trace "@@@ Unsolved Formula (rfsaConsistencyTest) @@@" $
                  Failed empty defect
    where
        candidates = pairsWithFilter (\u1 u2 -> maybeIf (row t u2 `sublang` row t u1) (u1, u2)) ss ss
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (not (tableAt t (u1 ++ [a]) v) /\ tableAt t (u2++[a]) v) (a:v)) candidates aa ee

constructHypothesisBollig :: NominalType i => State i -> Automaton (BRow i) i
constructHypothesisBollig State{..} = automaton q a d i f
    where
        q = primesUpp
        a = aa
        i = filter (\r -> r `sublang` row t []) q
        f = filter (\r -> singleton True `eq` mapFilter (\(i,b) -> maybeIf (i `eq` []) b) r) q
        d0 = triplesWithFilter (\s a s2 -> maybeIf (row t s2 `sublang` rowa t s a) (row t s, a, row t s2)) ss aa ss
        d = filter (\(q1,a,q2) -> q1 `member` q /\ q2 `member` q) d0
        primesUpp = filter (\r -> nonEmpty r /\ r `neq` rowUnion (sublangs r (allRows \\ orbit [] r))) allRowsUpp
        nonEmpty = isNotEmpty . filter (fromBool . Prelude.snd)
        allRowsUpp = map (row t) ss
        allRows = allRowsUpp `union` map (row t) ssa

makeCompleteBollig :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteBollig = makeCompleteWith [rfsaClosednessTest2, rfsaConsistencyTest]

learnBollig :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnBollig teacher = learn makeCompleteBollig useCounterExampleMP constructHypothesisBollig teacher initial
    where initial = constructEmptyState teacher
