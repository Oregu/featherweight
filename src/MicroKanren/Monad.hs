module MicroKanren.Monad where

import Control.Monad

import MicroKanren (LVar(LVar), CanUnify, Subst, unify)

newtype Logic α = Logic { runLogic :: [α] }
type SC α    = ([Subst α], Integer)
type Goal α  = (SC α → Logic α)

instance Monad Logic where
  Logic s >>= g = if null s then mzero else g (head s) `mplus` ((Logic $ tail s) >>= g)
  return = Logic . flip (:) (runLogic mzero)

instance MonadPlus Logic where
  mzero = Logic []
  mplus (Logic s1) (Logic s2) = Logic $ if null s1 then s2 else head s1 : mplus s2 (tail s1)

extS ∷ LVar a → LVar a → [Subst a] → [Subst a]
extS x v = (:) (x, v)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Goal α
(===) u v sc = maybe (Logic mzero) (\s → return (s, snd sc)) $ unify u v (fst sc)

disj ∷ Goal a → Goal a → Goal a
disj g1 g2 sc = g1 sc `mplus` g2 sc

conj ∷ Goal α → Goal α → Goal α
conj g1 g2 sc = g1 sc >>= g2

callFresh ∷ (LVar α → Goal β) → Goal β
callFresh f sc = let c = snd sc in f (LVar c) (fst sc, c+1)
