module MicroKanren.ExampleMonadic where

import Control.Monad

import MicroKanren (LVar(LVal))
import MicroKanren.Monad

emptyS ∷ SC α
emptyS = (mzero, 0)

aANDb ∷ Goal Integer
aANDb = conj
  (callFresh (\a -> a === LVal 7))
  (callFresh (\b -> disj (b === LVal 5) (b === LVal 6)))

runEx1, runEx2 ∷ Logic (SC Integer)
runEx1 = callFresh (\q -> q === LVal 5) emptyS
runEx2 = aANDb emptyS

fives, sixes ∷ LVar Integer -> Goal Integer
fives x = disj (x === LVal 5) (fives x)
sixes x = disj (x === LVal 6) (sixes x)

runFives, run5and6 ∷ Logic (SC Integer)
runFives = callFresh fives emptyS
run5and6 = callFresh (\x -> disj (fives x) (sixes x)) emptyS
