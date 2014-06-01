module MicroKanren.Example where

import MicroKanren

emptyS ∷ SC a
emptyS = ([], 0)

aANDb ∷ SC Integer → [SC Integer]
aANDb = conj
  (callFresh (\a -> a === Val 7))
  (callFresh (\b -> disj (b === Val 5) (b === Val 6)))

ex1, ex2 ∷ [SC Integer]
ex1 = callFresh (\q -> q === Val 5) emptyS
ex2 = aANDb emptyS

fives, sixes ∷ Var Integer -> SC Integer -> [SC Integer]
fives x = disj (x === Val 5) (fives x)
sixes x = disj (x === Val 6) (sixes x)

runFives, fivesAndSixes ∷ [SC Integer]
runFives = callFresh fives emptyS
fivesAndSixes = callFresh (\x -> disj (fives x) (sixes x)) emptyS

-- https://github.com/jasonhemann/microKanren/blob/master/microKanren-test.scm

--data Cons a = Nil | Cons (Var a) (Cons a) deriving (Eq, Show)
--
--appendo ∷ Eq a ⇒ Var (Cons a) → Var (Cons a) → Var (Cons a) → ([Subst (Cons a)], Integer) -> [([Subst (Cons a)], Integer)]
--appendo l s out =
--  disj
--    (conj (l === Val Nil) (s === out))
--    (callFresh (\a ->
--        callFresh (\d ->
--          conj
--            (l === Val Nil) -- Cons a d
--            (callFresh (\res →
--              (conj
--                (out === Val Nil) -- $ Cons a res
--                ((\sc → (appendo d s res) sc))))))))
--
--testAppendo = callFresh (\q → appendo q (Val $ Cons 1 Nil) (Val Nil)) emptyS
