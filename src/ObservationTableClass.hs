{-# language FunctionalDependencies #-}
{-# language TypeFamilies #-}

module ObservationTableClass where

import NLambda (NominalType, Set, pairsWith)
import Prelude ((++))

-- Words are indices to our table
type RowIndex i = [i]
type ColumnIndex i = [i]

-- Membership queries (TODO: move to Teacher)
type MQ i o = Set [i] -> Set ([i], o)

-- This is a fat class, so that instances could give more efficient implementations
class (NominalType table, NominalType i, NominalType o) => ObservationTable table i o | table -> i o where
  -- The type of data in a row is determined by the table
  type Row table :: *

  -- getters
  rows :: table -> Set (RowIndex i)
  cols :: table -> Set (ColumnIndex i)
  alph :: table -> Set i
  row :: table -> RowIndex i -> Row table

  -- perhaps not needed
  tableAt :: table -> RowIndex i -> ColumnIndex i -> Set o

  -- compound getters
  rowsExt :: table -> Set (RowIndex i)
  colsExt :: table -> Set (ColumnIndex i)

  rowEps :: table -> Row table

  -- updaters
  addRows :: MQ i o -> Set (RowIndex i) -> table -> table
  addColumns :: MQ i o -> Set (ColumnIndex i) -> table -> table

  -- default implementations
  rowsExt t = pairsWith (\r a -> r ++ [a]) (rows t) (alph t)
  colsExt t = pairsWith (:) (alph t) (cols t)
  rowEps t = row t []
