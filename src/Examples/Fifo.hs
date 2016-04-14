{-# LANGUAGE DeriveGeneric #-}
module Examples.Fifo (DataInput(..), fifoExample) where

import           GHC.Generics (Generic)
import           NLambda
import           Prelude      (Bool (..), Eq, Int, Maybe (..), Ord, Show,
                               length, reverse, ($), (+), (-), (.), (>=))
import qualified Prelude


-- Functional queue data type. First list is for push stuff onto, the
-- second list is to pop. If the second list is empty, it will reverse
-- the first.
data Fifo a = Fifo [a] [a] deriving (Eq, Ord, Show, Generic)

push :: a -> Fifo a -> Fifo a
push x (Fifo l1 l2) = Fifo (x:l1) l2

pop :: Fifo a -> Maybe (a, Fifo a)
pop (Fifo [] [])     = Nothing
pop (Fifo l1 [])     = pop (Fifo [] (reverse l1))
pop (Fifo l1 (x:l2)) = Just (x, Fifo l1 l2)

isEmptyFifo :: Fifo a -> Bool
isEmptyFifo (Fifo [] []) = True
isEmptyFifo _            = False

emptyFifo :: Fifo a
emptyFifo = Fifo [] []

sizeFifo :: Fifo a -> Int
sizeFifo (Fifo l1 l2) = length l1 + length l2


-- Our automaton accepts valid traces for this data structure. For
-- example Put(a) Get(a) is valid, but Put(a) Get(b) is only valid if
-- a = b. We will bound the number of states to make it a finite orbit
-- nominal automaton.

-- The alphabet:
data DataInput = Put Atom | Get Atom deriving (Eq, Ord, Show, Generic)
instance BareNominalType DataInput
instance Contextual DataInput where
    when f (Put a) = Put (when f a)
    when f (Get a) = Get (when f a)

-- The automaton: States consist of fifo queues and a sink state.
-- This representation is not minimal at all, but that's OK, since the
-- learner will learn a minimal anyways. The parameter n is the bound.
instance BareNominalType a => BareNominalType (Fifo a)
fifoExample n = automaton
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
    (singleton (Just emptyFifo))
    -- final states
    (map Just allStates)
    where
        allStates = sum . fromList $ [states i | i <- [0..n]]
        states i = sum . fromList $ [ pairsWith Fifo (replicateAtoms j) (replicateAtoms (i - j)) | j <- [0 .. i]]
        nextState fifo a = if sizeFifo fifo >= n
            then Nothing
            else Just (push a fifo)
        prevStates fifo = case pop fifo of
            Nothing -> map (\a -> (Just fifo, Get a, Nothing)) atoms
            Just (b, fifo2) -> singleton (Just fifo, Get b, Just fifo2)
                    `union` mapFilter (\a -> maybeIf (a `neq` b) (Just fifo, Get a, Nothing)) atoms
