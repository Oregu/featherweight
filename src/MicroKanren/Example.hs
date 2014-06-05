module MicroKanren.Example where

import MicroKanren

emptyS ∷ SC a
emptyS = ([], 0)

aANDb ∷ Goal Integer
aANDb = conj
  (callFresh (\a -> a === LVal 7))
  (callFresh (\b -> disj (b === LVal 5) (b === LVal 6)))

ex1, ex2 ∷ [SC Integer]
ex1 = callFresh (\q -> q === LVal 5) emptyS
ex2 = aANDb emptyS

fives, sixes ∷ LVar Integer -> Goal Integer
fives x = disj (x === LVal 5) (fives x)
sixes x = disj (x === LVal 6) (sixes x)

runFives, run5and6 ∷ [SC Integer]
runFives = callFresh fives emptyS
run5and6 = callFresh (\x -> disj (fives x) (sixes x)) emptyS

runCons ∷ [SC (LCons Integer)]
runCons = callFresh (\x -> disj (x === LVal (LCons (LVal (LCell 2)) (LVal Nil))) (x === LVal Nil)) emptyS

appendo ∷ Eq a ⇒ LVar (LCons a) → LVar (LCons a) → LVar (LCons a) → Goal (LCons a)
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

runAppendo ∷ [SC (LCons Integer)]
runAppendo = callFresh (\q →
  appendo
    q
    (LVal (LCons (LVal (LCell 5)) (LVal Nil)))
    (LVal (LCons (LVal (LCell 1)) (LVal (LCons (LVal (LCell 5)) (LVal Nil)))))) emptyS

runAppendo2 ∷ [SC (LCons Integer)]
runAppendo2 = callFresh (\q →
  appendo
    q q
    (LVal (LCons (LVal (LCell 2)) (LVal (LCons (LVal (LCell 1)) (LVal Nil)))))) emptyS
