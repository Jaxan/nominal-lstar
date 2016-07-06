{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module ObservationTable where

import           NLambda      hiding (fromJust)
import           Teacher

import           Control.DeepSeq (NFData, force)
import           Data.Maybe   (fromJust)
import           Debug.Trace  (trace)
import           GHC.Generics (Generic)
import           Prelude      (Bool (..), Eq, Ord, Show (..), ($), (++), (.), uncurry, id)
import qualified Prelude      ()


-- We represent functions as their graphs
type Fun a b = Set (a, b)

-- Basic manipulations on functions
-- Note that this returns a set, rather than an element
-- because we cannot extract a value from a singleton set
apply :: (NominalType a, NominalType b) => Fun a b -> a -> Set b
apply f a1 = mapFilter (\(a2, b) -> maybeIf (eq a1 a2) b) f

-- Returns the subset (of the domain) which exhibits
-- different return values for the two functions
discrepancy :: (NominalType a, NominalType b) => Fun a b -> Fun a b -> Set a
discrepancy f1 f2 =
    pairsWithFilter (
        \(a1,b1) (a2,b2) -> maybeIf (eq a1 a2 /\ neq b1 b2) a1
    ) f1 f2


-- An observation table is a function S x E -> O
-- (Also includes SA x E -> O)
type Table i o = Fun ([i], [i]) o
type Row i o = Fun [i] o

-- This is a rather arbitrary set of constraints
-- But I use them *everywhere*, so let's define them once and for all.
type LearnableAlphabet i = (NFData i, Contextual i, NominalType i, Show i)

-- `row is` denotes the data of a single row
-- that is, the function E -> O
row :: (NominalType i, NominalType o) => Table i o -> [i] -> Fun [i] o
row t is = mapFilter (\((a,b),c) -> maybeIf (eq is a) (b,c)) t

-- `rowa is a` is the row for the one letter extensions
rowa :: (NominalType i, NominalType o) => Table i o -> [i] -> i -> Fun [i] o
rowa t is a = row t (is ++ [a])

tableAt :: NominalType i => Table i Bool -> [i] -> [i] -> Formula
tableAt t s e = singleton True `eq` mapFilter (\((a,b),c) -> maybeIf (s `eq` a /\ b `eq` e) c) t

-- Teacher is restricted to Bools at the moment
type BTable i = Table i Bool
type BRow i = Row i Bool

-- fills part of the table. First parameter is the rows (with extension),
-- second is columns. Although the teacher provides us formulas instead of
-- booleans, we can partition the answers to obtain actual booleans.
fillTable :: LearnableAlphabet i => Teacher i -> Set [i] -> Set [i] -> BTable i
fillTable teacher sssa ee = Prelude.uncurry union . map2 (map slv) . map2 simplify . partition (\(_, _, f) -> f) $ base
    where
        base0 = pairsWith (\s e -> (s++e)) sssa ee
        base1 = membership teacher base0
        base1b s e = forAll id $ mapFilter (\(i,f) -> maybeIf (i `eq` (s++e)) f)  base1
        base = pairsWith (\s e -> (s, e, base1b s e)) sssa ee
        map2 f (a, b) = (f a, f b)
        slv (a,b,f) = ((a,b), fromJust . solve $ f)

-- Data structure representing the state of the learning algorithm (NOT a
-- state in the automaton)
data State i = State
    { t   :: BTable i -- the table
    , ss  :: Set [i]  -- state sequences
    , ssa :: Set [i]  -- their one letter extensions
    , ee  :: Set [i]  -- suffixes
    , aa  :: Set i    -- alphabet (remains constant)
    }
    deriving (Show, Ord, Eq, Generic, NFData, BareNominalType)

instance NominalType i => Conditional (State i) where
    cond f s1 s2 = fromTup (cond f (toTup s1) (toTup s2)) where
        toTup State{..} = (t,ss,ssa,ee,aa)
        fromTup (t,ss,ssa,ee,aa) = State{..}

instance (Ord i, Contextual i) => Contextual (State i) where
    when f s = fromTup (when f (toTup s)) where
        toTup State{..} = (t,ss,ssa,ee,aa)
        fromTup (t,ss,ssa,ee,aa) = State{..}

-- Precondition: the set together with the current rows is prefix closed
addRows :: LearnableAlphabet i => Teacher i -> Set [i] -> State i -> State i
addRows teacher ds0 state@State{..} =
    trace ("add rows: " ++ show ds) $
    state {t = t `union` dt, ss = ss `union` ds, ssa = ssa `union` dsa}
    where
        -- first remove redundancy
        ds = ds0 \\ ss
        -- extensions of new rows
        dsa = pairsWith (\s a -> s ++ [a]) ds aa
        -- For the new rows, we fill the table
        -- note that `ds ee` is already filled
        dt = fillTable teacher dsa ee

addColumns :: LearnableAlphabet i => Teacher i -> Set [i] -> State i -> State i
addColumns teacher de0 state@State{..} =
    trace ("add columns: " ++ show de) $
    state {t = t `union` dt, ee = ee `union` de}
    where
        -- first remove redundancy
        de = de0 \\ ee
        -- Fill that part of the table
        dt = fillTable teacher (ss `union` ssa) de
