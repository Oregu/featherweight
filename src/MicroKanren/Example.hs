{-# LANGUAGE RankNTypes #-}
module MicroKanren.Example where

import MicroKanren.Core

emptyS = ([], 0)

ex1 = callFresh (\q -> q === Val "5") emptyS

aANDb = conj
  (callFresh (\a -> a === Val "7"))
  (callFresh (\b -> disj (b === Val "5") (b === Val "6")))

ex2 = aANDb emptyS

fives x = disj (x === Val "5") (fives x)
runFives = callFresh fives emptyS

sixes x = disj (x === Val "6") (sixes x)
fivesAndSixes = callFresh (\x -> disj (fives x) (sixes x))

appendo = undefined
-- https://github.com/jasonhemann/microKanren/blob/master/microKanren-test.scm
