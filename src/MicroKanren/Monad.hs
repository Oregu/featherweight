module MicroKanren.Monad where

import Control.Monad

import MicroKanren (LVar(LVar), CanUnify, Subst, unify)

newtype Logic α = Logic { runLogic :: [SC α] }
type SC α    = ([Subst α], Integer)
type Goal α  = (SC α → Logic α)

instance Monad Logic where
  s >>= g = if null (runLogic s) then mzero else g (head (runLogic s)) `mplus` ((return $ tail (runLogic s)) >>= g)
  return = Logic . flip (:) (runLogic mzero)

instance MonadPlus Logic where
  mzero = Logic []
  mplus (Logic s1) (Logic s2) = Logic $ if null s1 then s2 else head s1 : mplus s2 (tail s1)

extS ∷ LVar a → LVar a → [Subst a] → [Subst a]
extS x v = (:) (x, v)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Goal α
(===) u v sc = let s = unify u v (fst sc) in
  maybe mzero (\s' → return (s', snd sc)) s

disj ∷ Goal a → Goal a → Goal a
disj g1 g2 sc = g1 sc `mplus` g2 sc

conj ∷ Goal α → Goal α → Goal α
conj g1 g2 sc = g1 sc >>= g2

callFresh ∷ (LVar α → Goal β) → Goal β
callFresh f sc = let c = snd sc in f (LVar c) (fst sc, c+1)
