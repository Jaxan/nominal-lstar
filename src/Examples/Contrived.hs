{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module Examples.Contrived where

import NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import GHC.Generics (Generic)
import Prelude (Eq, Ord, Show, ($))
import qualified Prelude ()

-- Example automaton from the whiteboard. Three orbits with 0, 1 and 2
-- registers. The third orbit has a local symmetry (S2).
data Example1 = Initial | S1 Atom | S2 (Atom, Atom)
  deriving (Show, Eq, Ord, Generic, Nominal, Contextual)

example1 :: Automaton Example1 Atom
example1 = automaton
    -- states, 4 orbits (of which one unreachable)
    (singleton Initial
        `union` map S1 atoms
        `union` map S2 atomsPairs)
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Initial, a, S1 a)) atoms
        `union` map (\a -> (S1 a, a, Initial)) atoms
        `union` mapFilter (\(a, b) -> maybeIf (neq a b) (S1 a, b, S2 (a, b))) atomsPairs
        `union` mapFilter (\(a, b, c) -> maybeIf (eq a c \/ eq b c) (S2 (a, b), c, Initial)) atomsTriples
        `union` mapFilter (\(a, b, c) -> maybeIf (neq a c /\ neq b c) (S2 (a, b), c, S1 c)) atomsTriples)
    -- initial states
    (singleton Initial)
    -- final states
    (map S2 atomsPairs)


-- Accepts all even words (ignores the alphabet). Two orbits, with a
-- trivial action. No registers.
data Aut2 = Even | Odd
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

example2 :: Automaton Aut2 Atom
example2 = automaton
    -- states, two orbits
    (fromList [Even, Odd])
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Even, a, Odd)) atoms
        `union` map (\a -> (Odd, a, Even)) atoms)
    -- initial states
    (singleton Even)
    -- final states
    (singleton Even)


-- Accepts all non-empty words with the same symbol. Three orbits: the initial
-- state, a state with a register and a sink state.
data Aut3 = Empty | Stored Atom | Sink
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

example3 :: Automaton Aut3 Atom
example3 = automaton
    -- states, three orbits
    (fromList [Empty, Sink]
        `union` map Stored atoms)
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Empty, a, Stored a)) atoms
        `union` map (\a -> (Stored a, a, Stored a)) atoms
        `union` map (\(a,b) -> (Stored a, b, Sink)) differentAtomsPairs
        `union` map (\a -> (Sink, a, Sink)) atoms)
    -- initial states
    (singleton Empty)
    -- final states
    (map Stored atoms)


-- Example showing that a local symmetry is not always the full symmetric group
-- or trivial. Five (reachable) orbits. The state Symm a b c has a C3 symmetry,
-- i.e. we can shift: Symm a b c ~ Symm b c a, but not swap: Symm a b c !~
-- Symm a c b (here ~ means bisimilar).
data Aut4 = Aut4Init              -- Initial state
          | First Atom            -- State after reading 1 symbol
          | Second Atom Atom      -- After reading two different symbols
          | Symm Atom Atom Atom   -- Accepting state with C3 symmetry
          | Sorted Atom Atom Atom -- State without symmetry
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

example4 :: Automaton Aut4 Atom
example4 = automaton
    -- states
    (singleton Aut4Init
        `union` map First atoms
        `union` map (unc2 Second) atomsPairs
        `union` map (unc3 Symm) atomsTriples
        `union` map (unc3 Sorted) atomsTriples)
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Aut4Init, a, First a)) atoms
        `union` map (\a -> (First a, a, Aut4Init)) atoms
        `union` map (\(a, b) -> (First a, b, Second a b)) differentAtomsPairs
        `union` map (\(a, b) -> (Second a b, a, Aut4Init)) atomsPairs
        `union` map (\(a, b) -> (Second a b, b, Aut4Init)) atomsPairs
        `union` mapFilter (\(a, b, c) -> maybeIf (c `neq` a /\ c `neq` b) (Second a b, c, Symm a b c)) atomsTriples
        `union` mapFilter (\(a, b, c, d) -> maybeIf (d `neq` a /\ d `neq` b /\ d `neq` c) (Symm a b c, d, Aut4Init)) atomsQuadruples
        `union` map (\(a, b, c) -> (Symm a b c, a, Sorted a b c)) atomsTriples
        `union` map (\(a, b, c) -> (Symm a b c, b, Sorted b c a)) atomsTriples
        `union` map (\(a, b, c) -> (Symm a b c, c, Sorted c a b)) atomsTriples
        `union` mapFilter (\(a, b, c, d) -> maybeIf (d `neq` a /\ d `neq` b /\ d `neq` c) (Sorted a b c, d, First d)) atomsQuadruples
        `union` map (\(a, b, c) -> (Sorted a b c, a, Sorted a b c)) atomsTriples
        `union` map (\(a, b, c) -> (Sorted a b c, b, Symm a b c)) atomsTriples
        `union` map (\(a, b, c) -> (Sorted a b c, c, Aut4Init)) atomsTriples)
    -- initial states
    (singleton Aut4Init)
    -- final states
    (map (unc3 Symm) atomsTriples)
    where
        atomsQuadruples = map (\[a,b,c,d] -> (a,b,c,d)) $ replicateAtoms 4
        unc2 f (a,b) = f a b
        unc3 f (a,b,c) = f a b c


-- Accepts all two-symbols words with different atoms
data Aut5 = Aut5Init | Aut5Store Atom | Aut5T | Aut5F
    deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

example5 :: Automaton Aut5 Atom
example5 = automaton
    -- states
    (singleton Aut5Init
        `union` map Aut5Store atoms
        `union` singleton Aut5T
        `union` singleton Aut5F)
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Aut5Init, a, Aut5Store a)) atoms
        `union` map (\a -> (Aut5Store a, a, Aut5F)) atoms
        `union` map (\(a, b) -> (Aut5Store a, b, Aut5T)) differentAtomsPairs
        `union` map (\a -> (Aut5F, a, Aut5F)) atoms
        `union` map (\a -> (Aut5T, a, Aut5F)) atoms)
    -- initial states
    (singleton Aut5Init)
    -- final states
    (singleton Aut5T)
