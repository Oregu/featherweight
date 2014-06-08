module MicroKanren.Monad where

import Control.Monad

import MicroKanren (LVar(LVar, LVal), CanUnify, Subst, unifyTerm, walk)

newtype Logic α = Logic [α]
unLogic (Logic a) = a

type SC α    = ([Subst α], Integer)
type Goal α  = (SC α → Logic α)

instance Monad Logic where
  Logic s >>= g = Logic (concat $ map (unLogic . g) s)
  return sc = Logic [sc]

instance MonadPlus Logic where
  mzero = Logic []
  mplus (Logic s1) (Logic s2) = Logic $ if null s1 then s2 else head s1 : mplus s2 (tail s1)

extS ∷ LVar a → LVar a → [Subst a] → [Subst a]
extS x v = (:) (x, v)

(===) ∷ (Eq α, CanUnify α, SC β) ⇒ LVar α → LVar α → β → Logic β
(===) u v sc = maybe mzero (\s → return (s, snd sc)) $ unify u v (fst sc)

unify ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → [Subst α] → Maybe [Subst α]
unify u v s = unify' u' v' s
  where
    u' = walk u s
    v' = walk v s
    unify' u2@(LVar _) v2@(LVar _) s2 = Just $ if u2 == v2 then s2
                                                           else extS u2 v2 s2
    unify' u2@(LVar _) v2          s2 = Just $ extS u2 v2 s2
    unify' u2         v2@(LVar _)  s2 = Just $ extS v2 u2 s2
    unify' (LVal u2)   (LVal v2)   s2 =   unifyTerm u2 v2 s2

disj ∷ Goal a → Goal a → Goal a
disj g1 g2 sc = g1 sc `mplus` g2 sc

conj ∷ Goal α → Goal α → Goal α
conj g1 g2 sc = g1 sc >>= g2

callFresh ∷ (LVar α → Goal β) → Goal β
callFresh f sc = let c = snd sc in f (LVar c) (fst sc, c+1)
