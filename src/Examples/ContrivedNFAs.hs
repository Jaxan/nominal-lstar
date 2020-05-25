{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TupleSections #-}
module Examples.ContrivedNFAs where

import NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import GHC.Generics (Generic)
import Prelude (Eq, Int, Ord, Show, (+), (-))
import qualified Prelude ()

-- Language = u a v a w for any words u,v,w and atom a
-- The complement of 'all distinct atoms'
-- Not determinizable
data NFA1 = Initial1 | Guessed1 Atom | Final1
  deriving (Show, Eq, Ord, Generic, NominalType, Contextual)

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


-- The typical example of a smal NFA: the n-last symbol was something nice
-- We will use the first symbol as distinguished atom
-- The DFA (in sets) will have something like 2^n states
-- whereas the NFA will be linear in n.
-- So this one *is* determinizable.
-- Also used in the Bollig et al paper.
data NFA2 = Initial2 | Distinguished Atom | Count Int
  deriving (Show, Eq, Ord, Generic, NominalType, Contextual)

exampleNFA2 :: Int -> Automaton NFA2 Atom
exampleNFA2 n = automaton
    (singleton Initial2
        `union` map Distinguished atoms
        `union` fromList [Count i | i <- [0 .. n]])
    atoms
    (map (\a -> (Initial2, a, Distinguished a)) atoms
        `union` pairsWith (\a b -> (Distinguished a, b, Distinguished a)) atoms atoms
        `union` map (\a -> (Distinguished a, a, Count 0)) atoms
        `union` sum (fromList [map (Count i, , Count (i+1)) atoms | i <- [0 .. n-1]]))
    (singleton Initial2)
    (singleton (Count n))

