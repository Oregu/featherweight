module MicroKanren.Example where

import MicroKanren.Core

emptyS ∷ ([Subst a], Integer)
emptyS = ([], 0)

aANDb ∷ ([Subst String], Integer) → [([Subst String], Integer)]
aANDb = conj
  (callFresh (\a -> a === Val "7"))
  (callFresh (\b -> disj (b === Val "5") (b === Val "6")))

ex1, ex2 ∷ [([Subst String], Integer)]
ex1 = callFresh (\q -> q === Val "5") emptyS
ex2 = aANDb emptyS

fives, sixes ∷ Var String -> ([Subst String], Integer) -> [([Subst String], Integer)]
fives x = disj (x === Val "5") (fives x)
sixes x = disj (x === Val "6") (sixes x)

runFives, fivesAndSixes ∷ [([Subst String], Integer)]
runFives = callFresh fives emptyS
fivesAndSixes = callFresh (\x -> disj (fives x) (sixes x)) emptyS

-- https://github.com/jasonhemann/microKanren/blob/master/microKanren-test.scm

--appendo l s out =
--  disj
--    (conj (l === []) (s === out))
--    (callFresh (\a ->
--        callFresh (\d ->
--          conj
--            ((a:d) === l)
--            (callFresh (\res →
--              (conj
--                ((a:res) === out)
--                ((\sc → (appendo d s res) sc))))))))
