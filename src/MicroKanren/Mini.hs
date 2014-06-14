module MicroKanren.Mini where

import Control.Monad
import Control.Monad.Trans.State

import MicroKanren.Monad

conde ∷ [Logic α] → Logic α
conde = msum

emptyS ∷ SC α
emptyS = (mzero, 0)

run ∷ Logic α → [SC α]
run l = unFairList $ execStateT l emptyS

runOnce ∷ Logic α -> [SC α]
runOnce = take 1 . run

runMany ∷ Int → Logic α -> [SC α]
runMany n = take n . run

reify ∷ a
reify = undefined
