{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TupleSections #-}
module Examples.RunningExample where

{- In this file we define the running example of the paper
   The language is L_n = { ww | w \in A^n } for any alphabet A.
   In terms of orbits, the minimal acceptor is quite large,
   but in terms of FO definable sets it is quite small.
-}

import NLambda hiding (alphabet)

import Data.List (reverse)
import GHC.Generics (Generic)
import Prelude (Eq, Int, Ord, Show, ($), (-))
import qualified Prelude ()


data RunningExample a = Store [a] | Check [a] | Accept | Reject
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

runningExample :: Nominal a => Set a -> Int -> Automaton (RunningExample a) a
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
    (unions [pairsWith storeTrans alphabet (iwords i) | i <- [0..depth-2]]
        `union` pairsWith betweenTrans alphabet (iwords (depth-1))
        `union` unions [pairsWith checkGoodTrans alphabet (iwords i) | i <- [1..depth-1]]
        `union` unions [triplesWithFilter checkBadTrans alphabet alphabet (iwords i) | i <- [1..depth-1]]
        `union` map (\a -> (Check [a], a, Accept)) alphabet
        `union` pairsWithFilter (\a b -> maybeIf (a `neq` b) (Check [a], b, Reject)) alphabet alphabet
        `union` map (Accept,, Reject) alphabet
        `union` map (Reject,, Reject) alphabet)
    (singleton $ Store [])
    (singleton Accept)
    where
        -- these define the states
        firstStates = unions [map Store $ iwords i | i <- [0..depth-1]]
        secondStates = unions [map Check $ iwords i | i <- [1..depth]]
        iwords i = replicateSet i alphabet
        -- this is the general shape of an transition
        storeTrans a l = (Store l, a, Store (a:l))
        betweenTrans a l = (Store l, a, Check (reverse (a:l)))
        checkGoodTrans a l = (Check (a:l), a, Check l)
        checkBadTrans a b l = maybeIf (a `neq` b) (Check (a:l), b, Reject)
