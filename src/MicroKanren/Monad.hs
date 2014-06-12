module MicroKanren.Monad where

import Control.Monad
import Control.Monad.State

import MicroKanren (LVar(LVar), CanUnify, unify)

type Logic α = StateT (SC α) [] α

type Subst α = (α, α)
type SC    α = ([Subst α], Integer)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Logic (LVar α)
(===) u v = do
            (s, c) ← get
            maybe mzero (\s' → put (s', c)) $ unify u v s
            return u

fresh ∷ Logic (LVar α)
fresh = do (_, cv) ← get
           modify $ \(s, c) → (s, c+1)
           return $ LVar cv
