{-# LANGUAGE RecordWildCards #-}
module NLStar where

import           AbstractLStar
import           Angluin
import           Bollig
import           ObservationTable
import           Teacher

import           NLambda

import           Debug.Trace
import           Data.List        (inits, tails)
import           Prelude          hiding (and, curry, filter, lookup, map, not,
                                   sum)

{- This is not NL* from the Bollig et al paper. This is a more abstract version
   wich uses different notions for closedness and consistency.
   Joshua argues this version is closer to the categorical perspective.
-}

-- lifted row functions
rowP t = rowUnion . map (row t)
rowPa t set a = rowUnion . map (\s -> rowa t s a) $ set

-- We can determine its completeness with the following
-- It returns all witnesses (of the form sa) for incompleteness
nonDetClosednessTest :: NominalType i => State i -> TestResult i
nonDetClosednessTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not closed" $ Failed defect empty
    where
        sss = map (rowP t) . hackApproximate $ ss
        -- true if the sequence sa has an equivalent row in ss
        hasEqRow = contains sss . row t
        defect = filter (not . hasEqRow) ssa

nonDetConsistencyTest :: NominalType i => State i -> TestResult i -- Set ((Set [i], Set [i], i), Set [i])
nonDetConsistencyTest State{..} = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty columns
    where
        rowPairs = pairsWithFilter (\s1 s2 -> maybeIf (candidate0 s1 s2) (s1,s2)) (hackApproximate ss) (hackApproximate ss)
        candidate0 s1 s2 = s1 `neq` s2 /\ rowP t s1 `eq` rowP t s2
        candidate1 s1 s2 a = rowPa t s1 a `neq` rowPa t s2 a
        defect = pairsWithFilter (
                     \(s1, s2) a -> maybeIf (candidate1 s1 s2 a) ((s1, s2, a), discrepancy (rowPa t s1 a) (rowPa t s2 a))
                 ) rowPairs aa
        columns = sum $ map (\((s1,s2,a),es) -> map (a:) es) defect

-- Given a C&C table, constructs an automaton. The states are given by 2^E (not
-- necessarily equivariant functions)
constructHypothesisNonDet :: NominalType i => State i -> Automaton (BRow i) i
constructHypothesisNonDet State{..} = automaton q a d i f
    where
        q = map (row t) ss
        a = aa
        d = triplesWithFilter (\s a s2 -> maybeIf (row t s2 `sublang` rowa t s a) (row t s, a, row t s2)) ss aa ss
        i = singleton $ row t []
        f = mapFilter (\s -> maybeIf (toform $ apply t (s, [])) (row t s)) ss
        toform s = forAll id . map fromBool $ s

makeCompleteNonDet :: LearnableAlphabet i => TableCompletionHandler i
makeCompleteNonDet = makeCompleteWith [nonDetClosednessTest, nonDetConsistencyTest]

-- Default: use counter examples in columns, which is slightly faster
learnNonDet :: LearnableAlphabet i => Teacher i -> Automaton (BRow i) i
learnNonDet teacher = learn makeCompleteNonDet useCounterExampleMP constructHypothesisNonDet teacher initial
    where initial = constructEmptyState teacher
