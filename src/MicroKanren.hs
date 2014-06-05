module MicroKanren where

import Data.Maybe (isJust, fromJust)
import Data.List  (find)

data LVar a = LVar Integer | LVal a deriving (Eq)

instance Show a ⇒ Show (LVar a) where
  show (LVar i) = "_" ++ show i
  show (LVal a) = show a

type Subst a = (LVar a, LVar a)
type SC    a = ([Subst a], Integer)
type Goal  a = SC a → [SC a]

class Eq α ⇒ CanUnify α where
  unifyTerm ∷ α → α → [Subst α] → Maybe [Subst α]
  unifyTerm u v s = if u == v then Just s else Nothing

instance CanUnify Integer

data LCons α = Nil
             | LCell α
             | LCons (LVar (LCons α)) (LVar (LCons α)) deriving (Eq, Show)

instance (Eq α) ⇒ CanUnify (LCons α) where
  unifyTerm (LCons u us) (LCons v vs) s = unify u v s >>= unify us vs
  unifyTerm u v s = if u == v then Just s else Nothing


walk ∷ Eq α ⇒ LVar α → [Subst α] → LVar α
walk u@(LVar _) s = let pr = find (\v → u == fst v) s in
  if isJust pr then walk (snd $ fromJust pr) s else u
walk u _ = u

extS ∷ LVar α → LVar α → [Subst α] → [Subst α]
extS x v = (:) (x, v)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Goal α
(===) u v sc = let s = unify u v (fst sc) in
  if isJust s then unit (fromJust s, snd sc) else mzero

mzero ∷ [SC α]
mzero = []

unit ∷ SC α → [SC α]
unit = flip (:) mzero

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

callFresh ∷ (LVar α → Goal β) → Goal β
callFresh f sc = let c = snd sc in f (LVar c) (fst sc, c+1)

disj ∷ Goal α → Goal α → Goal α
disj g1 g2 sc = mplus (g1 sc) (g2 sc)

conj ∷ Goal α → Goal α → Goal α
conj g1 g2 sc = bind (g1 sc) g2

mplus ∷ [SC α] → [SC α] → [SC α]
mplus s1 s2 = if null s1 then s2 else head s1 : mplus s2 (tail s1)

bind ∷ [SC α] → Goal α → [SC α]
bind s g = if null s then mzero else mplus (g $ head s) (bind (tail s) g)
