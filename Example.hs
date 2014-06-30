module Example where

import MicroKanren.Plain
import MicroKanren.Cons

emptyS ∷ SC α
emptyS = ([], 0)

aANDb ∷ Goal Int
aANDb = conj
  (callFresh (\a -> a === LVal 7))
  (callFresh (\b -> disj (b === LVal 5) (b === LVal 6)))

ex1, ex2 ∷ [SC Int]
ex1 = callFresh (\q -> q === LVal 5) emptyS
ex2 = aANDb emptyS

fives, sixes ∷ LVar Int -> Goal Int
fives x = disj (x === LVal 5) (fives x)
sixes x = disj (x === LVal 6) (sixes x)

runFives, run5and6 ∷ [SC Int]
runFives = callFresh fives emptyS
run5and6 = callFresh (\x -> disj (fives x) (sixes x)) emptyS

runCons ∷ [SC (LCons Int)]
runCons = callFresh
            (\x -> disj (x === LVal (fromList [2])) (x === LVal empty))
            emptyS
