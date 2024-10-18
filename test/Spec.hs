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
main = defaultMain (testGroup "Unit tests" unitTests)
  where
    unitTests =
        [ testCase "Learning DFA DW1" $ learnAndCompare (runningExample atoms 1) @?= True
        , testCase "Learning DFA DW2" $ learnAndCompare (runningExample atoms 2) @?= True
        , testCase "Mealy deterministic echo" $ isTrue (mealyIsDeterministic echoMachine) @?= True
        , testCase "Mealy deterministic memory True" $ isTrue (mealyIsDeterministic (memoryMachine True)) @?= True
        , testCase "Mealy deterministic memory False" $ isTrue (mealyIsDeterministic (memoryMachine False)) @?= True
        , testCase "Mealy enabled echo" $ isTrue (mealyIsEnabled echoMachine) @?= True
        , testCase "Mealy enabled memory True" $ isTrue (mealyIsEnabled (memoryMachine True)) @?= True
        , testCase "Mealy enabled memory False" $ isTrue (mealyIsEnabled (memoryMachine False)) @?= True
        ]

learnAndCompare :: (Contextual i, Show i, Nominal i, Nominal q1) => Automaton q1 i -> Bool
learnAndCompare target = equivalenceCheck target learnedModel
  where
    learnedModel = learnAngluin (teacherWithTarget target)
    equivalenceCheck m1 m2 = isTrue . isEmpty $ bisim m1 m2

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

echoMachine :: MealyMachine () Atom Atom
echoMachine = MealyMachine
    { mealyInitialState = ()
    , mealyStates = singleton ()
    , mealyInputs = atoms
    , mealyTransitions = NLambda.map (\a -> ((), a, a, ())) atoms
    }

data MIn a = MPut a | MGet
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

data MOut a = MNok | MOk | MVal a
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

memoryMachine :: Bool -> MealyMachine (Maybe Atom) (MIn Atom) (MOut Atom)
memoryMachine forget = MealyMachine
    { mealyInitialState = Nothing
    , mealyStates = allStates
    , mealyInputs = singleton MGet `union` NLambda.map MPut atoms
    , mealyTransitions = NLambda.pairsWith (\q a -> (q, MPut a, MOk, Just a)) allStates atoms `union`
                         NLambda.map (\a -> (Just a, MGet, MVal a, if forget then Nothing else Just a)) atoms `union`
                         NLambda.singleton (Nothing, MGet, MNok, Nothing)
    }
  where allStates = singleton Nothing `union` (NLambda.map Just atoms)
