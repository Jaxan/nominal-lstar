module Teachers.Whitebox where

import NLambda

import Prelude hiding (filter, map, not, sum)


-- Checks bisimulation of initial states (only for DFAs)
-- returns some counterexamples if not bisimilar
-- returns empty set iff bisimilar
bisim :: (NominalType i, NominalType q1, NominalType q2) => Automaton q1 i -> Automaton q2 i -> Set [i]
bisim aut1 aut2 = go empty (pairsWith addEmptyWord (initialStates aut1) (initialStates aut2))
    where
        go rel todo =
            let -- if elements are already in R, we can skip them
                todo2 = filter (\(_, x, y) -> (x, y) `notMember` rel) todo
                -- split into correct pairs and wrong pairs
                (cont, ces) = partition (\(_, x, y) -> (x `member` finalStates aut1) <==> (y `member` finalStates aut2)) todo2
                aa = NLambda.alphabet aut1
                -- the good pairs should make one step
                dtodo = sum (pairsWith (\(w, x, y) a -> pairsWith (\x2 y2 -> (a:w, x2, y2)) (d aut1 a x) (d aut2 a y)) cont aa)
            in  -- if there are wrong pairs
                ite (isNotEmpty ces)
                   -- then return counter examples
                   (map getRevWord ces)
                   -- else continue with good pairs
                   (ite (isEmpty dtodo) empty (go (rel `union` map stripWord cont) dtodo))
        d aut a x = mapFilter (\(s, l, t) -> maybeIf (s `eq` x /\ l `eq` a) t) (delta aut)
        stripWord (_, x, y) = (x, y)
        getRevWord (w, _, _) = reverse w
        addEmptyWord x y = ([], x, y)

-- Attempt at using a bisimlution up to to proof bisimulation between NFAs.
-- Inspired by the Hacking non-determinism paper. However, they only
-- consider finite sums (which is enough for finite sets, but not for
-- nominal sets). Here, I have to do a bit of trickery to get all sums.
-- I am not sure about correctness, but that is not really an issue for our
-- use-case. Note that deciding equivalence of NFAs is undecidable, so we
-- bound the bisimulation depth.
bisimNonDet :: (Show i, Show q1, Show q2, NominalType i, NominalType q1, NominalType q2) => Int -> Automaton q1 i -> Automaton q2 i -> Set [i]
bisimNonDet n aut1 aut2 = go empty (singleton ([], initialStates aut1, initialStates aut2))
    where
        go rel todo0 =
            let -- if elements are too long, we ignore them
                todo0b = filter (\(w,_,_) -> fromBool (length w <= n)) todo0
                -- if elements are already in R, we can skip them
                todo1 = filter (\(_, x, y) -> (x, y) `notMember` rel) todo0b
                -- now we are going to do a up-to thingy
                -- we look at all subsets x2 of x occuring in R (similarly for y)
                xbar x = mapFilter (\(x2, _) -> maybeIf (x2 `isSubsetOf` x) x2) rel
                ybar y = mapFilter (\(_, y2) -> maybeIf (y2 `isSubsetOf` y) y2) rel
                -- and then the sums are expressed by these formulea kind of
                xform x y = x `eq` sum (xbar x) /\ forAll (\x2 -> exists (\y2 -> rel `contains` (x2, y2)) (ybar y)) (xbar x)
                yform x y = y `eq` sum (ybar y) /\ forAll (\y2 -> exists (\x2 -> rel `contains` (x2, y2)) (xbar x)) (ybar y)
                notSums x y = not (xform x y /\ yform x y)
                -- filter out things expressed as sums
                todo2 = filter (\(_, x, y) -> notSums x y) todo1
                -- split into correct pairs and wrong pairs
                (cont, ces) = partition (\(_, x, y) -> (x `intersect` finalStates aut1) <==> (y `intersect` finalStates aut2)) todo2
                aa = NLambda.alphabet aut1
                -- the good pairs should make one step
                dtodo = pairsWith (\(w, x, y) a -> (a:w, sumMap (d aut1 a) x, sumMap (d aut2 a) y)) cont aa
            in  -- if there are wrong pairs
                ite (isNotEmpty ces)
                    -- then return counter examples
                    (map getRevWord ces)
                    -- else continue with good pairs
                    (ite (isEmpty dtodo) empty (go (rel `union` map stripWord cont) dtodo))
        d aut a x = mapFilter (\(s, l, t) -> maybeIf (s `eq` x /\ l `eq` a) t) (delta aut)
        stripWord (_, x, y) = (x, y)
        getRevWord (w, _, _) = reverse w
        sumMap f = sum . map f
