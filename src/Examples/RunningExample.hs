{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TupleSections #-}
module Examples.RunningExample where

{- In this file we define the running example of the paper
   The language is L_n = { ww | w \in A^n } for any alphabet A.
   In terms of orbits, the minimal acceptor is quite large,
   but in terms of FO definable sets it is quite small.
-}

import NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import Data.List (reverse)
import Prelude (Eq, Int, Ord, Show, ($), (-), (.))
import qualified Prelude ()

import GHC.Generics (Generic)

-- Parametric in the alphabet, because why not?
data RunningExample a = Store [a] | Check [a] | Accept | Reject
  deriving (Eq, Ord, Show, Generic, NominalType, Contextual)

runningExample :: NominalType a => Set a -> Int -> Automaton (RunningExample a) a
runningExample alphabet 0 = automaton
    (fromList [Accept, Reject])
    alphabet
    (map (Accept,,Reject) alphabet `union` map (Reject,,Reject) alphabet)
    (singleton Accept)
    (singleton Accept)
runningExample alphabet depth = automaton
    (firstStates
        `union` secondStates
        `union` singleton Accept
        `union` singleton Reject)
    alphabet
    (sums [pairsWith storeTrans alphabet (iwords i) | i <- [0..depth-2]]
        `union` pairsWith betweenTrans alphabet (iwords (depth-1))
        `union` sums [pairsWith checkGoodTrans alphabet (iwords i) | i <- [1..depth-1]]
        `union` sums [triplesWithFilter checkBadTrans alphabet alphabet (iwords i) | i <- [1..depth-1]]
        `union` map (\a -> (Check [a], a, Accept)) alphabet
        `union` pairsWithFilter (\a b -> maybeIf (a `neq` b) (Check [a], b, Reject)) alphabet alphabet
        `union` map (Accept,, Reject) alphabet
        `union` map (Reject,, Reject) alphabet)
    (singleton $ Store [])
    (singleton Accept)
    where
        -- these define the states
        firstStates = sums [map Store $ iwords i | i <- [0..depth-1]]
        secondStates = sums [map Check $ iwords i | i <- [1..depth]]
        iwords i = replicateSet i alphabet
        -- this is the general shape of an transition
        storeTrans a l = (Store l, a, Store (a:l))
        betweenTrans a l = (Store l, a, Check (reverse (a:l)))
        checkGoodTrans a l = (Check (a:l), a, Check l)
        checkBadTrans a b l = maybeIf (a `neq` b) (Check (a:l), b, Reject)

sums :: NominalType a => [Set a] -> Set a
sums = sum . fromList
