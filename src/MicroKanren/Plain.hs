module MicroKanren.Plain where

import Control.Monad
import Data.List (find)

data LVar α = LVar Integer | LVal α deriving (Eq)

instance Show α ⇒ Show (LVar α) where
  show (LVar i) = "_" ++ show i
  show (LVal a) = show a

type Subst α = (LVar α, LVar α)
type SC    α = ([Subst α], Integer)
type Goal  α = SC α → [SC α]

class Eq α ⇒ CanUnify α where
  unifyTerm ∷ α → α → [Subst α] → Maybe [Subst α]
  unifyTerm u v s = if u == v then Just s else Nothing

instance CanUnify Int
instance CanUnify Integer


fairMplus ∷ [α] → [α] → [α]
fairMplus s1 s2 = if null s1 then s2 else head s1 : fairMplus s2 (tail s1)

walk ∷ Eq α ⇒ LVar α → [Subst α] → LVar α
walk u@(LVar _) s = maybe u (\v → walk (snd v) s) $ find (\v → u == fst v) s
walk u _ = u

extS ∷ LVar α → LVar α → [Subst α] → [Subst α]
extS x v s = (x, v) : s

unify ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → [Subst α] → Maybe [Subst α]
unify u v s = unify' u' v' s
  where
    u' = walk u s
    v' = walk v s
    unify' u2@(LVar _) v2@(LVar _) s2 = Just $ if u2 == v2 then s2
                                                           else extS u2 v2 s2
    unify' u2@(LVar _) v2          s2 = Just $ extS u2 v2 s2
    unify' u2          v2@(LVar _) s2 = Just $ extS v2 u2 s2
    unify' (LVal u2)   (LVal v2)   s2 =   unifyTerm u2 v2 s2

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → SC α → [SC α]
(===) u v sc = maybe mzero (\s' → return (s', snd sc)) (unify u v (fst sc))

callFresh ∷ (LVar α → Goal α) → Goal α
callFresh f sc = let c = snd sc in f (LVar c) (fst sc, c+1)

disj ∷ Goal α → Goal α → Goal α
disj g1 g2 sc = fairMplus (g1 sc) (g2 sc)

conj ∷ Goal α → Goal α → Goal α
conj g1 g2 sc = g1 sc >>= g2
