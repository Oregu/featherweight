module MicroKanren.ExampleMonadic where

import Control.Monad
import Control.Monad.State.Lazy

import MicroKanren (LVar(LVal))
import MicroKanren.Monad

emptyS ∷ SC α
emptyS = (mzero, 0)

aANDb ∷ Logic (LVar Integer)
aANDb = do
    a ← fresh
    b ← fresh
    (a === LVal 7)
    (mplus (b === LVal 5)
           (b === LVal 6))

run ∷ StateT (SC α) L α -> L (α, SC α)
run g = runStateT g emptyS

runEx1, runEx2 ∷ L (LVar Integer, SC (LVar Integer))
runEx1 = run $ do
    q ← fresh
    (q === LVal 5) ∷ Logic (LVar Integer)
runEx2 = run aANDb

--fives, sixes ∷ LVar Integer -> Goal Integer
--fives x = mplus (x === LVal 5) (fives x)
--sixes x = mplus (x === LVal 6) (sixes x)

--runFives, run5and6 ∷ Logic (SC Integer)
--runFives = callFresh fives emptyS
--run5and6 = callFresh (\x -> mplus (fives x) (sixes x)) emptyS
