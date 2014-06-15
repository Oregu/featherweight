module MicroKanren.Mini where

import Control.Monad
import Control.Monad.Trans.State

import MicroKanren (LVar(LVar, LVal), walk)
import MicroKanren.Monad

conde ∷ [Logic α] → Logic α
conde = msum

emptyS ∷ SC α
emptyS = (mzero, 0)

run ∷ Eq α ⇒ Logic (LVar α) → [LVar α]
run l = reify $ unFairList $ runStateT l emptyS

runOnce ∷ Eq α ⇒ Logic (LVar α) -> [LVar α]
runOnce = take 1 . run

runMany ∷ Eq α ⇒ Int → Logic (LVar α) -> [LVar α]
runMany n = take n . run

run' ∷ Logic α → [SC α]
run' l = unFairList $ execStateT l emptyS

reify ∷ Eq α ⇒ [(LVar α, SC (LVar α))] → [LVar α]
reify = map reifyVar
  where
    reifyVar (lv@(LVal _), _) = lv
    reifyVar (lv@(LVar vi), sc) =
      let s = fst sc
       in case (walk lv s) of
        LVal x → LVal x
        LVar i → if (i == vi) then lv else reifyVar (LVar i, sc)
