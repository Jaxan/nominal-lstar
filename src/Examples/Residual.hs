{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module Examples.Residual
  ( exampleResidual1
  , exampleResidual2
  ) where

import Examples.Fifo (DataInput (..))
import NLambda

import GHC.Generics (Generic)
import Prelude (Eq, Ord, Read, Show)
import qualified Prelude ()

data Res1 a = QR1 a | QR2 | QAncStar
  deriving (Eq, Ord, Show, Generic, NominalType, Contextual)

exampleResidual1 :: Automaton (Res1 Atom) DataInput
exampleResidual1 = automaton
    -- state space
    (fromList [QR2, QAncStar]
        `union` map QR1 atoms)
    -- alphabet
    (map Put atoms `union` map Get atoms)
    -- transition relation
    (map (\a -> (QR1 a, Get a, QR1 a)) atoms
        `union` pairsWithFilter (\a b -> maybeIf (a `neq` b) (QR1 a, Put b, QR1 a)) atoms atoms
        `union` map (\a -> (QR1 a, Put a, QR2)) atoms
        `union` map (\a -> (QAncStar, Put a, QAncStar)) atoms)
    -- initial states
    (map QR1 atoms `union` singleton QAncStar)
    -- final states
    (fromList [QR2, QAncStar])


-- Example when learning breaks
data Res2 a = Guess a | GuessConfused a | Accept
  deriving (Eq, Ord, Show, Generic, NominalType, Contextual)

data AlphabetR a = A a | Anc a
  deriving (Eq, Ord, Show, Read, Generic, NominalType, Contextual)

exampleResidual2 :: Automaton (Res2 Atom) (AlphabetR Atom)
exampleResidual2 = automaton
    -- state space
    (singleton Accept
        `union` map Guess atoms
        `union` map GuessConfused atoms)
    -- alphabet
    (map Anc atoms `union` map A atoms)
    -- transition relation
    (map (\a -> (Guess a, A a, Accept)) atoms
        `union` map (\a -> (GuessConfused a, A a, Accept)) atoms
        `union` map (\a -> (Guess a, Anc a, Accept)) atoms
        `union` map (\a -> (GuessConfused a, Anc a, Accept)) atoms
        `union` pairsWithFilter (\a b -> maybeIf (a `neq` b) (Guess a, A b, Guess a)) atoms atoms
        `union` pairsWithFilter (\a b -> maybeIf (a `neq` b) (GuessConfused a, A b, GuessConfused a)) atoms atoms
        `union` pairsWithFilter (\a b -> maybeIf (a `neq` b) (Guess a, Anc b, GuessConfused a)) atoms atoms
        `union` map (\a -> (GuessConfused a, A a, Guess a)) atoms)
    -- initial states
    (map Guess atoms)
    -- final states
    (fromList [Accept])
