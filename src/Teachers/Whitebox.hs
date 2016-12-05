module Teachers.Whitebox where

import NLambda

import Control.Monad.Identity
import Prelude hiding (map, sum, filter, not)

-- I found it a bit easier to write a do-block below. So I needed this
-- Conditional instance.
instance Conditional a => Conditional (Identity a) where
    cond f x y = return (cond f (runIdentity x) (runIdentity y))


-- Checks bisimulation of initial states (only for DFAs)
-- returns some counter examples if not bisimilar
-- returns empty set iff bisimilar
bisim :: (NominalType i, NominalType q1, NominalType q2) => Automaton q1 i -> Automaton q2 i -> Set [i]
bisim aut1 aut2 = runIdentity $ go empty (pairsWith addEmptyWord (initialStates aut1) (initialStates aut2))
    where
        go rel todo = do
            -- if elements are already in R, we can skip them
            let todo2 = filter (\(_, x, y) -> (x, y) `notMember` rel) todo
            -- split into correct pairs and wrong pairs
            let (cont, ces) = partition (\(_, x, y) -> (x `member` (finalStates aut1)) <==> (y `member` (finalStates aut2))) todo2
            let aa = NLambda.alphabet aut1
            -- the good pairs should make one step
            let dtodo = sum (pairsWith (\(w, x, y) a -> pairsWith (\x2 y2 -> (a:w, x2, y2)) (d aut1 a x) (d aut2 a y)) cont aa)
            -- if there are wrong pairs
            ite (isNotEmpty ces)
                -- then return counter examples
                (return $ map getRevWord ces)
                -- else continue with good pairs
                (ite (isEmpty dtodo)
                    (return empty)
                    (go (rel `union` map stripWord cont) (dtodo))
                )
        d aut a x = mapFilter (\(s, l, t) -> maybeIf (s `eq` x /\ l `eq` a) t) (delta aut)
        stripWord (_, x, y) = (x, y)
        getRevWord (w, _, _) = reverse w
        addEmptyWord x y = ([], x, y)

-- Attempt at using a bisimlution up to to proof bisimulation between NFAs
-- Because why not? Inspired by the Hacking non-determinism paper
-- But they only consider finite sums (which is enough for finite sets)
-- Here I have to do a bit of trickery, which is hopefully correct.
-- I think it is correct, but not yet complete enough, we need more up-to.
bisimNonDet :: (Show i, Show q1, Show q2, NominalType i, NominalType q1, NominalType q2) => Int -> Automaton q1 i -> Automaton q2 i -> Set [i]
bisimNonDet n aut1 aut2 = runIdentity $ go empty (singleton ([], initialStates aut1, initialStates aut2))
    where
        go rel todo0 = do
            -- if elements are too long, we ignore them
            let todo0b = filter (\(w,_,_) -> fromBool (length w <= n)) todo0
            -- if elements are already in R, we can skip them
            let todo1 = filter (\(_, x, y) -> (x, y) `notMember` rel) todo0b
            -- now we are going to do a up-to thingy
            -- we look at all subsets x2 of x occuring in R (similarly for y)
            let xbar x = mapFilter (\(x2, _) -> maybeIf (x2 `isSubsetOf` x) x2) rel
            let ybar y = mapFilter (\(_, y2) -> maybeIf (y2 `isSubsetOf` y) y2) rel
            -- and then the sums are expressed by these formulea kind of
            let xform x y = x `eq` sum (xbar x) /\ forAll (\x2 -> exists (\y2 -> rel `contains` (x2, y2)) (ybar y)) (xbar x)
            let yform x y = y `eq` sum (ybar y) /\ forAll (\y2 -> exists (\x2 -> rel `contains` (x2, y2)) (xbar x)) (ybar y)
            let notSums x y = not (xform x y /\ yform x y)
            -- filter out things expressed as sums
            let todo2 = filter (\(_, x, y) -> notSums x y) todo1
            -- split into correct pairs and wrong pairs
            let (cont, ces) = partition (\(_, x, y) -> (x `intersect` (finalStates aut1)) <==> (y `intersect` (finalStates aut2))) todo2
            let aa = NLambda.alphabet aut1
            -- the good pairs should make one step
            let dtodo = pairsWith (\(w, x, y) a -> (a:w, sumMap (d aut1 a) x, sumMap (d aut2 a) y)) cont aa
            -- if there are wrong pairs
            --trace "go" $ traceShow rel $ traceShow todo0 $ traceShow todo1 $ traceShow todo2 $ traceShow cont $
            ite (isNotEmpty ces)
                -- then return counter examples
                (return $ map getRevWord ces)
                -- else continue with good pairs
                (ite (isEmpty dtodo)
                    (return empty)
                    (go (rel `union` map stripWord cont) (dtodo))
                )
        d aut a x = mapFilter (\(s, l, t) -> maybeIf (s `eq` x /\ l `eq` a) t) (delta aut)
        stripWord (_, x, y) = (x, y)
        getRevWord (w, _, _) = reverse w
        addEmptyWord x y = ([], x, y)
        sumMap f = sum . (map f)
