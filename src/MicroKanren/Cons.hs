module MicroKanren.Cons (
  LCons
, empty
, singleton
, fromList
, cons
) where

import MicroKanren (CanReify, reifyVar)
import MicroKanren.Plain (LVar(..), CanUnify, unify, unifyTerm)

data LCons α = Nil
             | LCell α
             | LCons (LVar (LCons α)) (LVar (LCons α)) deriving (Eq, Show)

instance CanUnify α ⇒ CanUnify (LCons α) where
  unifyTerm (LCons u us) (LCons v vs) s = unify u v s >>= unify us vs
  unifyTerm u v s = if u == v then Just s else Nothing

instance CanReify α ⇒ CanReify (LCons α) where
  reifyVar  Nil         sc = Nil
  reifyVar (LCell a)    sc = LCell a
  reifyVar (LCons a as) sc = LCons a as-- (reifyVar a sc) (reifyVar as sc)

fromList ∷ [α] → LCons α
fromList []     = Nil
fromList (a:as) = LCons (LVal $ LCell a) (LVal $ fromList as)

singleton ∷ α → LCons α
singleton a = LCons (LVal $ LCell a) (LVal Nil)

empty ∷ LCons α
empty = Nil

cons ∷ LVar (LCons α) → LVar (LCons α) → LCons α
cons c cs = LCons c cs
