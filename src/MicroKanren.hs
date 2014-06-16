module MicroKanren where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import MicroKanren.Plain (LVar(LVar, LVal), CanUnify, unify, fairMplus, walk)

newtype FairList α = FairList { unFairList :: [α] } deriving (Eq, Show, Functor, Applicative, Monad, Alternative)

instance MonadPlus FairList where
  s1 `mplus` s2 = FairList $ unFairList s1 `fairMplus` unFairList s2
  mzero = FairList []

type Logic α = StateT (SC α) FairList α

type Subst α = (α, α)
type SC    α = ([Subst α], Integer)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Logic (LVar α)
(===) u v = do
            (s, c) ← get
            maybe mzero (\s' → put (s', c)) $ unify u v s
            return u

-- === Should return () actually, but Logic type tightens var type
-- and a list of substitutions. I want substitutions to be polymorphic
-- so I can remove LCell type constructor from LCons. (It's a temp hack.)

fresh ∷ Logic (LVar α)
fresh = do (_, cv) ← get
           modify $ \(s, c) → (s, c+1)
           return $ LVar cv

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

class Eq α ⇒ CanReify α where
  reifyVar ∷ α → SC α → α

instance Eq α ⇒ CanReify (LVar α) where
  reifyVar lv@(LVal _)   _ = lv
  reifyVar lv@(LVar vi) sc =
    let s = fst sc
     in case (walk lv s) of
        LVal x → LVal x
        LVar i → if (i == vi) then lv else reifyVar (LVar i) sc

reify ∷ CanReify α ⇒ [(α, SC α)] → [α]
reify = map (uncurry reifyVar)
