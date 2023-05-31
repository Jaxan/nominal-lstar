{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module Examples.Stack (DataInput(..), stackExample) where

import NLambda hiding (states)

import Examples.Fifo (DataInput (..))
import GHC.Generics (Generic)
import Prelude (Eq, Int, Maybe (..), Ord, Show, length, ($), (.), (>=))
import qualified Prelude ()


-- Functional stack data type is simply a list.
newtype Stack a = Stack [a]
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

push :: a -> Stack a -> Stack a
push x (Stack l1) = Stack (x:l1)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack [])    = Nothing
pop (Stack (x:l)) = Just (x, Stack l)

emptyStack :: Stack a
emptyStack = Stack []

sizeStack :: Stack a -> Int
sizeStack (Stack l1) = length l1


-- Our automaton accepts valid traces for this data structure. For
-- example Put(a) Get(a) is valid, but Put(a) Get(b) is only valid if
-- a = b. We will bound the number of states to make it a finite orbit
-- nominal automaton.

-- The alphabet is defined in Examples.Fifo

-- The automaton: States consist of stacks and a sink state.
-- The parameter n is the bound.
stackExample :: Int -> Automaton (Maybe (Stack Atom)) DataInput
stackExample n = automaton
    -- states
    (singleton Nothing
        `union` map Just allStates)
    -- alphabet
    (map Put atoms `union` map Get atoms)
    -- transitions
    (map (\a -> (Nothing, Put a, Nothing)) atoms
        `union` map (\a -> (Nothing, Get a, Nothing)) atoms
        `union` pairsWith (\s a -> (Just s, Put a, nextState s a)) allStates atoms
        `union` sum (map prevStates allStates))
    -- initial states
    (singleton (Just emptyStack))
    -- final states
    (map Just allStates)
    where
        allStates = sum . fromList $ [states i | i <- [0..n]]
        states i = map Stack (replicateAtoms i)
        nextState stack a = if sizeStack stack >= n
            then Nothing
            else Just (push a stack)
        prevStates stack = case pop stack of
            Nothing -> map (\a -> (Just stack, Get a, Nothing)) atoms
            Just (b, stack2) -> singleton (Just stack, Get b, Just stack2)
                    `union` mapFilter (\a -> maybeIf (a `neq` b) (Just stack, Get a, Nothing)) atoms
