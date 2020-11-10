{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language RecordWildCards #-}

module SimpleObservationTable where

import NLambda hiding (fromJust)

import GHC.Generics (Generic)
import Prelude (Bool (..), Eq, Int, Ord, Show (..), fst, (++))
import qualified Prelude ()


-- We represent functions as their graphs
-- Except when o = Bool, more on that later
type Fun i o = Set (i, o)

dom :: (NominalType i, NominalType o) => Fun i o -> Set i
dom = map fst

-- Words are indices to our table
type RowIndex i = [i]
type ColumnIndex i = [i]

-- A table is nothing more than a part of the language.
-- Invariant: content is always defined for elements in
-- `rows * columns` and `rows * alph * columns`.
data Table i o = Table
    { content :: Fun [i] o
    , rows    :: Set (RowIndex i)
    , columns :: Set (ColumnIndex i)
    , alph    :: Set i
    }
    deriving (Show, Ord, Eq, Generic, NominalType, Conditional, Contextual)

rowsExt :: (NominalType i, NominalType o) => Table i o -> Set (RowIndex i)
rowsExt Table{..} = pairsWith (\r a -> r ++ [a]) rows alph

columnsExt :: (NominalType i, NominalType o) => Table i o -> Set (RowIndex i)
columnsExt Table{..} = pairsWith (:) alph columns

-- I could make a more specific implementation for booleans
-- But for now we reuse the above.
type BTable i = Table i Bool

-- A row is the data in a table, i.e. a function from columns to the output
type Row i o = Fun [i] o

row :: (NominalType i, NominalType o) => Table i o -> RowIndex i -> Row i o
row Table{..} r = pairsWithFilter (\e (a, b) -> maybeIf (a `eq` (r ++ e)) (e, b)) columns content

-- Special case of a boolean: functions to Booleans are subsets
type BRow i = Set [i]

-- TODO: slightly inefficient
brow :: NominalType i => BTable i -> RowIndex i -> BRow i
brow Table{..} r = let lang = mapFilter (\(i, o) -> maybeIf (fromBool o) i) content
                   in filter (\a -> lang `contains` (r ++ a)) columns


-- Membership queries (TODO: move to Teacher)
type MQ i o = Set [i] -> Set ([i], o)

initialTableWith :: (NominalType i, NominalType o) => MQ i o -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> Table i o
initialTableWith mq alphabet newRows newColumns = Table
    { content = content
    , rows = newRows
    , columns = newColumns
    , alph = alphabet
    }
    where
        newColumnsExt = pairsWith (:) alphabet newColumns
        domain = pairsWith (++) newRows (newColumns `union` newColumnsExt)
        content = mq domain

initialTable :: (NominalType i, NominalType o) => MQ i o -> Set i -> Table i o
initialTable mq alphabet = initialTableWith mq alphabet (singleton []) (singleton [])

initialTableSize :: (NominalType i, NominalType o) => MQ i o -> Set i -> Int -> Int -> Table i o
initialTableSize mq alphabet rs cs = initialTableWith mq alphabet (replicateSetUntil rs alphabet) (replicateSetUntil cs alphabet)

-- Assumption: newRows is disjoint from rows (for efficiency)
addRows :: (NominalType i, NominalType o) => MQ i o -> Set (RowIndex i) -> Table i o -> Table i o
addRows mq newRows t@Table{..} =
    t { content = content `union` newContent
      , rows = rows `union` newRows
      }
    where
        newRowsExt = pairsWith (\r a -> r ++ [a]) newRows alph
        newPart = pairsWith (++) (newRows `union` newRowsExt) columns
        newPartRed = newPart \\ dom content
        newContent = mq newPartRed

-- Assumption: newColumns is disjoint from columns (for efficiency)
addColumns :: (NominalType i, NominalType o) => MQ i o -> Set (ColumnIndex i) -> Table i o -> Table i o
addColumns mq newColumns t@Table{..} =
    t { content = content `union` newContent
      , columns = columns `union` newColumns
      }
    where
        newColumnsExt = pairsWith (:) alph newColumns
        newPart = pairsWith (++) rows (newColumns `union` newColumnsExt)
        newPartRed = newPart \\ dom content
        newContent = mq newPartRed
