module Example where

import MicroKanren

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
runCons = callFresh (\x -> disj (x === LVal (LCons (LVal (LCell 2)) (LVal Nil))) (x === LVal Nil)) emptyS

appendo ∷ Eq α ⇒ LVar (LCons α) → LVar (LCons α) → LVar (LCons α) → Goal (LCons α)
appendo l s out =
  disj
    (conj (l === LVal Nil) (s === out))
    (callFresh (\h →
        callFresh (\t →
          conj
            (l === LVal (LCons h t))
            (callFresh (\res →
              conj
                (out === LVal (LCons h res))
                (appendo t s res))))))

runAppendo ∷ [SC (LCons Int)]
runAppendo = callFresh (\q →
  appendo
    q
    (LVal (LCons (LVal (LCell 5)) (LVal Nil)))
    (LVal (LCons (LVal (LCell 1)) (LVal (LCons (LVal (LCell 5)) (LVal Nil)))))) emptyS

runAppendo2 ∷ [SC (LCons Int)]
runAppendo2 = callFresh (\q →
  appendo
    q q
    (LVal (LCons (LVal (LCell 1)) (LVal (LCons (LVal (LCell 1)) (LVal Nil)))))) emptyS
