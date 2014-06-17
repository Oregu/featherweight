module MicroKanren.Plain where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

data LVar a = LVar Integer | LVal a deriving (Eq)

instance Show a ⇒ Show (LVar a) where
  show (LVar i) = "_" ++ show i
  show (LVal a) = show a

data Subst = forall α. CanUnify α ⇒ Subst (Map (LVar α) (LVar α))
type SC    = (Subst, Integer)
type Goal  = SC → [SC]

class Eq α ⇒ CanUnify α where
  unifyTerm ∷ α → α → Subst → Maybe Subst
  unifyTerm u v s = if u == v then Just s else Nothing

instance CanUnify Int
instance CanUnify Integer

data LCons α = Nil
             | LCons (LVar α) (LVar (LCons α)) deriving (Eq, Show)

instance (Eq α, CanUnify α) ⇒ CanUnify (LCons α) where
  unifyTerm (LCons u us) (LCons v vs) s = unify u v s >>= unify us vs
  unifyTerm u v s = if u == v then Just s else Nothing


fairMplus ∷ [α] → [α] → [α]
fairMplus s1 s2 = if null s1 then s2 else head s1 : fairMplus s2 (tail s1)

walk ∷ (Eq α, CanUnify α) ⇒ LVar α → Subst → LVar α
walk u@(LVar _) (Subst s) = maybe u (\v → walk v (Subst s)) $ Map.lookup u s
walk u _ = u

extS ∷ CanUnify α ⇒ LVar α → LVar α → Subst → Subst
extS x v (Subst s) = Subst $ Map.insert x v s

unify ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Subst → Maybe Subst
unify u v s = unify' u' v' s
  where
    u' = walk u s
    v' = walk v s
    unify' u2@(LVar _) v2@(LVar _) s2 = Just $ if u2 == v2 then s2
                                                           else extS u2 v2 s2
    unify' u2@(LVar _) v2          s2 = Just $ extS u2 v2 s2
    unify' u2          v2@(LVar _) s2 = Just $ extS v2 u2 s2
    unify' (LVal u2)   (LVal v2)   s2 =   unifyTerm u2 v2 s2

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → SC → [SC]
(===) u v sc = maybe mzero (\s' → return (s', snd sc)) (unify u v (fst sc))

callFresh ∷ (LVar α → Goal) → Goal
callFresh f sc = let c = snd sc in f (LVar c) (fst sc, c+1)

disj ∷ Goal → Goal → Goal
disj g1 g2 sc = fairMplus (g1 sc) (g2 sc)

conj ∷ Goal → Goal → Goal
conj g1 g2 sc = g1 sc >>= g2
