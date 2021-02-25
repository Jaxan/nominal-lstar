{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}

module BooleanObservationTable where

import ObservationTableClass

import GHC.Generics (Generic)
import NLambda
import Prelude (Bool (..), Eq, Int, Ord, Show (..), (++), (.))
import qualified Prelude ()

-- Helper function
mqToSubset :: NominalType i => (Set [i] -> Set ([i], Bool)) -> Set [i] -> Set [i]
mqToSubset mq = mapFilter (\(i, o) -> maybeIf (fromBool o) i) . mq

-- A table is nothing more than a part of the language.
-- Invariant: content is always a subset of
-- `domain` = `rows * columns` union `rows * alph * columns`.
data Table i = Table
    { content    :: Set [i]
    , domain     :: Set [i]
    , rowIndices :: Set (RowIndex i)
    , colIndices :: Set (ColumnIndex i)
    , aa         :: Set i
    }
    deriving (Show, Ord, Eq, Generic, NominalType, Conditional, Contextual)

instance (NominalType i, Contextual i) => ObservationTable (Table i) i Bool where
    type Row (Table i) = Set [i]
    rows = rowIndices
    cols = colIndices
    alph = aa
    row Table{..} r = filter (\e -> (r ++ e) `member` content) colIndices
    rowEps Table{..} = intersection content colIndices
    tableAt Table{..} r c = ite ((r ++ c) `member` content) (singleton True) (singleton False)

    addRows mq newRows t@Table{..} =
        t { content = content `union` newContent
          , domain = domain `union` newPartRed
          , rowIndices = rowIndices `union` newRows
          }
        where
            newRowsExt = pairsWith (\r a -> r ++ [a]) newRows aa
            newPart = pairsWith (++) (newRows `union` newRowsExt) colIndices
            newPartRed = newPart \\ domain
            newContent = mqToSubset mq newPartRed

    addColumns mq newColumns t@Table{..} =
        t { content = content `union` newContent
          , domain = domain `union` newPartRed
          , colIndices = colIndices `union` newColumns
          }
        where
            newColumnsExt = pairsWith (:) aa newColumns
            newPart = pairsWith (++) rowIndices (newColumns `union` newColumnsExt)
            newPartRed = newPart \\ domain
            newContent = mqToSubset mq newPartRed


initialBTableWith :: NominalType i => MQ i Bool -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> Table i
initialBTableWith mq alphabet newRows newColumns = Table
    { content = content
    , domain = domain
    , rowIndices = newRows
    , colIndices = newColumns
    , aa = alphabet
    }
    where
        newColumnsExt = pairsWith (:) alphabet newColumns
        domain = pairsWith (++) newRows (newColumns `union` newColumnsExt)
        content = mqToSubset mq domain

initialBTable :: NominalType i => MQ i Bool -> Set i -> Table i
initialBTable mq alphabet = initialBTableWith mq alphabet (singleton []) (singleton [])

initialBTableSize :: NominalType i => MQ i Bool -> Set i -> Int -> Int -> Table i
initialBTableSize mq alphabet rs cs = initialBTableWith mq alphabet (replicateSetUntil rs alphabet) (replicateSetUntil cs alphabet)
