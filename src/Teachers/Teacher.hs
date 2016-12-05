{-# LANGUAGE RankNTypes #-}
module Teachers.Teacher where

import NLambda
import Prelude hiding (map)

-- Abstract teacher type. Maybe this will be generalized to some monad, so that
-- the teacher can have state (such as a cache).
-- TODO: add a notion of state, so that Teachers can maintain a cache, or
--       a socket, or file, or whatever (as long as it stays deterministic).
data Teacher i = Teacher
    -- A teacher provides a way to answer membership queries. You'd expect
    -- a function of type [i] -> Bool. But in order to implement some teachers
    -- more efficiently, we provide Set [i] as input, and the teacher is
    -- supposed to answer each element in the set.
    { membership :: Set [i] -> Set ([i], Formula)
    -- Given a hypothesis, returns Nothing when equivalence or a (equivariant)
    -- set of counter examples. Needs to be quantified over q, because the
    -- learner may choose the type of the state space.
    , equivalent :: forall q. (Show q, NominalType q) => Automaton q i -> Maybe (Set [i])
    -- Returns the alphabet to the learner
    , alphabet   :: Set i
    }

-- Often a membership query is defined by a function [i] -> Formula. This wraps
-- such a function to the required type for a membership query (see above).
foreachQuery :: NominalType i => ([i] -> Formula) -> Set[i] -> Set ([i], Formula)
foreachQuery f qs = map (\q -> (q, f q)) qs
