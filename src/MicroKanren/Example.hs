module MicroKanren.Example where

import MicroKanren

emptyS ∷ SC a
emptyS = ([], 0)

aANDb ∷ Goal Integer
aANDb = conj
  (callFresh (\a -> a === Val 7))
  (callFresh (\b -> disj (b === Val 5) (b === Val 6)))

ex1, ex2 ∷ [SC Integer]
ex1 = callFresh (\q -> q === Val 5) emptyS
ex2 = aANDb emptyS

fives, sixes ∷ Var Integer -> Goal Integer
fives x = disj (x === Val 5) (fives x)
sixes x = disj (x === Val 6) (sixes x)

runFives, fivesAndSixes ∷ [SC Integer]
runFives = callFresh fives emptyS
fivesAndSixes = callFresh (\x -> disj (fives x) (sixes x)) emptyS

-- https://github.com/jasonhemann/microKanren/blob/master/microKanren-test.scm

data Cons a = Nil | Cons (Var a) (Var (Cons a)) deriving (Eq)
instance Show a ⇒ Show (Cons a) where
  show Nil = "[]"
  show (Cons h t) = show h ++ ":" ++ show t

runCons ∷ [SC (Cons Integer)]
runCons = callFresh (\x -> disj (x === Val (Cons (Val 2) (Val Nil))) (x === Val Nil)) emptyS

--appendo ∷ Var (Cons Integer) → Var (Cons Integer) → Var (Cons Integer) → Goal (Cons Integer)
--appendo l s out =
--  disj
--    (conj (l === Val Nil) (s === out))
--    (callFresh (\h →
--        callFresh (\t →
--          conj
--            (l === Val (Cons h t))
--            (callFresh (\res →
--              (conj
--                (out === Val (Cons h res))
--                (appendo t s res)))))))

--testAppendo ∷ [SC (Cons Integer)]
--testAppendo = callFresh (\q → appendo q (Val $ Cons (Val 1) (Val Nil)) (Val Nil)) emptyS
