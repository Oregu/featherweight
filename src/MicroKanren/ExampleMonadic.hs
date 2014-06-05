module MicroKanren.ExampleMonadic where

import Control.Monad

import MicroKanren (Var(Var, Val), Subst)
import MicroKanren.Monad

emptyS ∷ (Logic a, Integer)
emptyS = (mzero, 0)

aANDb ∷ (Logic Integer, Integer) → [(Logic Integer, Integer)]
aANDb = conj
  (callFresh (\a -> a === Val 7))
  (callFresh (\b -> disj (b === Val 5) (b === Val 6)))

ex1, ex2 ∷ [(Logic Integer, Integer)]
ex1 = callFresh (\q -> q === Val 5) emptyS
ex2 = aANDb emptyS

fives, sixes ∷ Var Integer -> (Logic Integer, Integer) -> [(Logic Integer, Integer)]
fives x = disj (x === Val 5) (fives x)
sixes x = disj (x === Val 6) (sixes x)

runFives, fivesAndSixes ∷ Logic Integer
runFives = callFresh fives emptyS
fivesAndSixes = callFresh (\x -> disj (fives x) (sixes x)) emptyS
