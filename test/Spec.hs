{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-partial-type-signatures #-}

import Angluin
import Examples
import Teacher
import Teachers.Whitebox

import GHC.Generics (Generic)
import NLambda
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Unit tests" [dfaLearning, mealyBasics])
  where
    dfaLearning =
        testGroup
            "DFA Learning"
            [ testCase "Learning DFA DW1" $ learnDFAAndCompare (runningExample atoms 1) @?= True
            , testCase "Learning DFA DW2" $ learnDFAAndCompare (runningExample atoms 2) @?= True
            ]
    mealyBasics =
        testGroup
            "Mealy Basics"
            [ testCase "Mealy deterministic echo" $ isTrue (mealyIsDeterministic echoMachine) @?= True
            , testCase "Mealy deterministic memory True" $ isTrue (mealyIsDeterministic (memoryMachine True)) @?= True
            , testCase "Mealy deterministic memory False" $ isTrue (mealyIsDeterministic (memoryMachine False)) @?= True
            , testCase "Mealy enabled echo" $ isTrue (mealyIsEnabled echoMachine) @?= True
            , testCase "Mealy enabled memory True" $ isTrue (mealyIsEnabled (memoryMachine True)) @?= True
            , testCase "Mealy enabled memory False" $ isTrue (mealyIsEnabled (memoryMachine False)) @?= True
            , testCase "Mealy reachable echo" $ isTrue (mealyReachable echoMachine `eq` singleton ()) @?= True
            , testCase "Mealy reachable memory True" $ isTrue (mealyReachable (memoryMachine True) `eq` (singleton Nothing `union` NLambda.map Just atoms)) @?= True
            , testCase "Mealy reachable memory False" $ isTrue (mealyReachable (memoryMachine False) `eq` (singleton Nothing `union` NLambda.map Just atoms)) @?= True
            ]

learnDFAAndCompare :: (Contextual i, Show i, Nominal i, Nominal q1) => Automaton q1 i -> Bool
learnDFAAndCompare target = equivalenceCheck target learnedModel
  where
    learnedModel = learnAngluin (teacherWithTarget target)
    equivalenceCheck m1 m2 = isTrue . isEmpty $ bisim m1 m2

{- *******************************************************************
   Basic data structure for Nominal Mealy machines and basic functions to
   check whether they are deterministic and input-enabled.
-}
data MealyMachine s i o = MealyMachine
    { mealyInitialState :: s
    , mealyStates :: Set s
    , mealyInputs :: Set i
    , mealyTransitions :: Set (s, i, o, s)
    }
    deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

mealyIsDeterministic :: _ => MealyMachine s i o -> Formula
mealyIsDeterministic m = forAll (\s -> forAll (\i -> let ot = mapFilter ((\(s2, i2, o, t) -> maybeIf ((s, i) `eq` (s2, i2)) (o, t))) (mealyTransitions m) in forAll (\(x, y) -> x `eq` y) (pairs ot ot)) (mealyInputs m)) (mealyStates m)

mealyIsEnabled :: _ => MealyMachine s i o -> Formula
mealyIsEnabled m = forAll (\s -> forAll (\i -> let ot = mapFilter ((\(s2, i2, o, t) -> maybeIf ((s, i) `eq` (s2, i2)) (o, t))) (mealyTransitions m) in isNotEmpty ot) (mealyInputs m)) (mealyStates m)

-- Computes all reachable states
mealyReachable :: _ => MealyMachine s i o -> Set s
mealyReachable m = go (singleton (mealyInitialState m)) empty
  where
    go todo visited =
      ite (isEmpty todo)
      {- then -} visited
      {- else -} (let v = visited `union` todo
                      s = successors todo
                    in go (s \\ v) v)
    successors todo = NLambda.sum (pairsWith (\s i -> mapFilter ((\(s2, i2, _, t) -> maybeIf ((s, i) `eq` (s2, i2)) t)) (mealyTransitions m)) todo (mealyInputs m))

{- *******************************************************************
   EXAMPLE MEALY MACHINES
-}

-- Immediately echoes the input (of type Atom)
echoMachine :: MealyMachine () Atom Atom
echoMachine =
    MealyMachine
        { mealyInitialState = ()
        , mealyStates = singleton ()
        , mealyInputs = atoms
        , mealyTransitions = NLambda.map (\a -> ((), a, a, ())) atoms
        }

-- The next example is a memory cell to store a single atom. You can Put
-- or Get the values. Depending on the mode, the machine can forget its
-- value after a Get operation.
data MIn a = MPut a | MGet
    deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

data MOut a = MNok | MOk | MVal a
    deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

memoryMachine :: Bool -> MealyMachine (Maybe Atom) (MIn Atom) (MOut Atom)
memoryMachine forget =
    MealyMachine
        { mealyInitialState = Nothing
        , mealyStates = allStates
        , mealyInputs = singleton MGet `union` NLambda.map MPut atoms
        , mealyTransitions =
            NLambda.pairsWith (\q a -> (q, MPut a, MOk, Just a)) allStates atoms
                `union` NLambda.map (\a -> (Just a, MGet, MVal a, if forget then Nothing else Just a)) atoms
                `union` NLambda.singleton (Nothing, MGet, MNok, Nothing)
        }
  where
    allStates = singleton Nothing `union` (NLambda.map Just atoms)
