module Functions where

import           NLambda
import           Prelude (($))
import qualified Prelude ()

-- We represent functions as their graphs
type Fun a b = Set (a, b)

-- Basic manipulations on functions
-- Note that this returns a set, rather than an element
-- because we cannot extract a value from a singleton set
apply :: (NominalType a, NominalType b) => Fun a b -> a -> Set b
apply f a1 = mapFilter (\(a2, b) -> maybeIf (eq a1 a2) b) f

-- AxB -> c is adjoint to A -> C^B
-- curry and uncurry witnesses both ways of the adjunction
curry :: (NominalType a, NominalType b, NominalType c) => Fun (a, b) c -> Fun a (Fun b c)
curry f = map (\a1 -> (a1, mapFilter (\((a2,b),c) -> maybeIf (eq a1 a2) (b,c)) f)) as
    where as = map (\((a, _), _) -> a) f

uncurry :: (NominalType a, NominalType b, NominalType c) => Fun a (Fun b c) -> Fun (a, b) c
uncurry f = sum $ map (\(a,s) -> map (\(b,c) -> ((a, b), c)) s) f

-- Returns the subset (of the domain) which exhibits
-- different return values for the two functions
-- I am not sure about its correctness...
discrepancy :: (NominalType a, NominalType b) => Fun a b -> Fun a b -> Set a
discrepancy f1 f2 =
    pairsWithFilter (
        \(a1,b1) (a2,b2) -> maybeIf (eq a1 a2 /\ neq b1 b2) a1
    ) f1 f2
