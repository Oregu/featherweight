module MicroKanren.Monad where

import Control.Monad
import Data.Maybe (isJust, fromJust)

import MicroKanren (LVar(LVar), Subst, unify)

data Logic a = L { unL :: [a] }
type SC a = ([Subst a], Integer)

instance Monad Logic where
  L s >>= g = if null s then mzero else g (head s) `mplus` ((L $ tail s) >>= g)
  return = L . flip (:) (unL mzero)

instance MonadPlus Logic where
  mzero = L []
  mplus (L s1) (L s2) = L $ if null s1 then s2 else head s1 : mplus s2 (tail s1)

extS ∷ Var a → Var a → [Subst a] → [Subst a]
extS x v = (:) (x, v)

(===) ∷ Eq a ⇒ Var a → Var a → SC a → [SC a]
(===) u v sc = let s = unify u v (fst sc) in
  if isJust s then return (fromJust s, snd sc) else mzero

disj ∷ (a → Logic b) → (a → Logic b) → SC a → Logic b
disj g1 g2 sc = g1 sc `mplus` g2 sc

conj ∷ (a → Logic b) → (b → Logic c) → a → Logic c
conj = (>=>)

callFresh ∷ (Var c → (Logic a, Integer) → b) → (Logic a, Integer) → b
callFresh f sc = let c = snd sc in f (Var c) (fst sc, c+1)
