module MicroKanren where

import Data.Maybe (isJust, fromJust)
import Data.List  (find)

data Var a = Var Integer | Val a deriving (Eq, Show)
type Subst a = (Var a, Var a)

walk ∷ Eq a ⇒ Var a → [Subst a] → Var a
walk u@(Var _) s = let pr = find (\v → u == fst v) s in
  if isJust pr then walk (snd $ fromJust pr) s else u
walk u _ = u

extS ∷ Var a → Var a → [Subst a] → [Subst a]
extS x v = (:) (x, v)

(===) ∷ Eq a ⇒ Var a → Var a → ([Subst a], t) → [([Subst a], t)]
(===) u v sc = let s = unify u v (fst sc) in
  if isJust s then unit (fromJust s, snd sc) else mzero

mzero ∷ [a]
mzero = []

unit ∷ a → [a]
unit = flip (:) mzero

unify ∷ Eq a ⇒ Var a → Var a → [Subst a] → Maybe [Subst a]
unify u v s =
  let u' = walk u s
      v' = walk v s
      unify' u2@(Var _) v2@(Var _) s2 = Just $ if u2 == v2 then s2
                                                           else extS u2 v2 s2
      unify' u2@(Var _) v2 s2 = Just $ extS u2 v2 s2
      unify' u2 v2@(Var _) s2 = Just $ extS v2 u2 s2
      unify' u2 v2 s2 = if u2 == v2 then Just s2 else Nothing
   in unify' u' v' s

callFresh ∷ (Var c → (a, Integer) → b) → (a, Integer) → b
callFresh f sc = let c = snd sc in f (Var c) (fst sc, c+1)

disj ∷ (a → [b]) → (a → [b]) → a → [b]
disj g1 g2 sc = mplus (g1 sc) (g2 sc)

conj ∷ (a → [b]) → (b → [c]) → a → [c]
conj g1 g2 sc = bind  (g1 sc)  g2

mplus ∷ [a] → [a] → [a]
mplus s1 s2 = if null s1 then s2 else head s1 : mplus s2 (tail s1)

bind ∷ [a] → (a → [b]) → [b]
bind s g = if null s then mzero else mplus (g $ head s) (bind (tail s) g)
