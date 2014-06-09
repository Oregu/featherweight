module MicroKanren.ExampleMonadic where

import Control.Monad

import MicroKanren (LVar(LVal))
import MicroKanren.Monad

emptyS ∷ SC α
emptyS = (mzero, 0)

aANDb ∷ Goal (LVar Integer)
aANDb = do
	a ← fresh
	b ← fresh
	a === LVal 7
	mplus (b === LVal 5)
	      (b === LVal 6)

--runEx1, runEx2 ∷ Logic (SC (LVar Integer))
--runEx1 = (fresh >>= (\q -> q === LVal 5)) emptyS
--runEx2 = aANDb emptyS

--fives, sixes ∷ LVar Integer -> Goal Integer
--fives x = mplus (x === LVal 5) (fives x)
--sixes x = mplus (x === LVal 6) (sixes x)

--runFives, run5and6 ∷ Logic (SC Integer)
--runFives = callFresh fives emptyS
--run5and6 = callFresh (\x -> mplus (fives x) (sixes x)) emptyS
