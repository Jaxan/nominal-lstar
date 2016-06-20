{-# LANGUAGE DeriveGeneric #-}
module Examples.ContrivedNFAs where

import           NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import           Prelude      (Eq, Ord, Show, ($))
import qualified Prelude      ()

import           GHC.Generics (Generic)

-- Language = u a v a w for any words u,v,w and atom a
-- The complement of 'all distinct atoms'
data NFA1 = Initial1 | Guessed1 Atom | Final1
  deriving (Show, Eq, Ord, Generic)
instance BareNominalType NFA1
exampleNFA1 :: Automaton NFA1 Atom
exampleNFA1 = automaton
    -- states, 4 orbits (of which one unreachable)
    (singleton Initial1
        `union` map Guessed1 atoms
        `union` singleton Final1)
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Initial1, a, Guessed1 a)) atoms
        `union` map (\a -> (Initial1, a, Initial1)) atoms
        `union` map (\a -> (Guessed1 a, a, Final1)) atoms
        `union` pairsWith (\a b -> (Guessed1 a, b, Guessed1 a)) atoms atoms
        `union` map (\a -> (Final1, a, Final1)) atoms)
    -- initial states
    (singleton Initial1)
    -- final states
    (singleton Final1)
