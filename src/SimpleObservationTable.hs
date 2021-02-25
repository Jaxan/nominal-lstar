{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PartialTypeSignatures #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module SimpleObservationTable where

import ObservationTableClass

import Data.Coerce (coerce)
import GHC.Generics (Generic)
import NLambda
import Prelude (Bool (..), Eq, Int, Ord, Show (..), fst, (++))
import qualified Prelude ()


-- We represent functions as their graphs
-- Except when o = Bool, more on that later
type Fun i o = Set (i, o)

dom :: (NominalType i, NominalType o) => Fun i o -> Set i
dom = map fst

-- A table is nothing more than a part of the language.
-- Invariant: content is always defined for elements in
-- `rows * columns` and `rows * alph * columns`.
data Table i o = Table
    { content    :: Fun [i] o
    , rowIndices :: Set (RowIndex i)
    , colIndices :: Set (ColumnIndex i)
    , aa         :: Set i
    }
    deriving (Show, Ord, Eq, Generic, NominalType, Conditional, Contextual)

instance (NominalType i, NominalType o) => ObservationTable (Table i o) i o where
    type Row (Table i o) = Fun [i] o
    rows = rowIndices
    cols = colIndices
    alph = aa
    row Table{..} r = pairsWithFilter (\e (a, b) -> maybeIf (a `eq` (r ++ e)) (e, b)) colIndices content
    tableAt Table{..} r c = mapFilter (\(i, o) -> maybeIf ((r ++ c) `eq` i) o) content

    -- Assumption: newRows is disjoint from rows (for efficiency)
    addRows mq newRows t@Table{..} =
        t { content = content `union` newContent
          , rowIndices = rowIndices `union` newRows
          }
        where
            newRowsExt = pairsWith (\r a -> r ++ [a]) newRows aa
            newPart = pairsWith (++) (newRows `union` newRowsExt) colIndices
            newPartRed = newPart \\ dom content
            newContent = mq newPartRed

    -- Assumption: newColumns is disjoint from columns (for efficiency)
    addColumns mq newColumns t@Table{..} =
        t { content = content `union` newContent
          , colIndices = colIndices `union` newColumns
          }
        where
            newColumnsExt = pairsWith (:) aa newColumns
            newPart = pairsWith (++) rowIndices (newColumns `union` newColumnsExt)
            newPartRed = newPart \\ dom content
            newContent = mq newPartRed


-- We can reuse the above tables for the Boolean case and
-- perform some minor optimisations.
newtype Boolean table = B { unB :: table }
    deriving (Show, Ord, Eq, Generic, NominalType, Conditional, Contextual)

type BTable i = Boolean (Table i Bool)

instance (NominalType i) => ObservationTable (BTable i) i Bool where
    -- Special case of a boolean: functions to Booleans are subsets
    type Row (BTable i) = Set [i]

    -- All the reusable functions are simply coerced
    rows = coerce (rows :: _ => Table i Bool -> _)
    cols = coerce (cols :: _ => Table i Bool -> _)
    rowsExt = coerce (rowsExt :: _ => Table i Bool -> _)
    colsExt = coerce (colsExt :: _ => Table i Bool -> _)
    alph = coerce (alph :: _ => Table i Bool -> _)
    tableAt = coerce (tableAt :: _ => Table i Bool -> _)
    addRows = coerce (addRows :: _ => _ -> _ -> Table i Bool -> Table i Bool)
    addColumns = coerce (addColumns :: _ => _ -> _ -> Table i Bool -> Table i Bool)

    -- These are specific to our representation of Row
    row (B Table{..}) r = let lang = mapFilter (\(i, o) -> maybeIf (fromBool o) i) content
                          in filter (\a -> lang `contains` (r ++ a)) colIndices
    rowEps (B Table{..}) = mapFilter (\(i, o) -> maybeIf (fromBool o /\ i `member` colIndices) i) content


initialTableWith :: (NominalType i, NominalType o) => MQ i o -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> Table i o
initialTableWith mq alphabet newRows newColumns = Table
    { content = content
    , rowIndices = newRows
    , colIndices = newColumns
    , aa = alphabet
    }
    where
        newColumnsExt = pairsWith (:) alphabet newColumns
        domain = pairsWith (++) newRows (newColumns `union` newColumnsExt)
        content = mq domain

initialTable :: (NominalType i, NominalType o) => MQ i o -> Set i -> Table i o
initialTable mq alphabet = initialTableWith mq alphabet (singleton []) (singleton [])

initialTableSize :: (NominalType i, NominalType o) => MQ i o -> Set i -> Int -> Int -> Table i o
initialTableSize mq alphabet rs cs = initialTableWith mq alphabet (replicateSetUntil rs alphabet) (replicateSetUntil cs alphabet)

initialBTableWith :: NominalType i => MQ i Bool -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> BTable i
initialBTableWith = coerce initialTableWith

initialBTable :: NominalType i => MQ i Bool -> Set i -> BTable i
initialBTable = coerce initialTable

initialBTableSize :: NominalType i => MQ i Bool -> Set i -> Int -> Int -> BTable i
initialBTableSize = coerce initialTableSize
