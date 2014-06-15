module MicroKanren.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import MicroKanren (LVar(LVar), CanUnify, unify, fairMplus)

newtype FairList α = FairList { unFairList :: [α] } deriving (Eq, Show, Functor, Applicative, Monad, Alternative)

instance MonadPlus FairList where
  s1 `mplus` s2 = FairList $ unFairList s1 `fairMplus` unFairList s2
  mzero = FairList []

type Logic α = StateT (SC α) FairList α

type Subst α = (α, α)
type SC    α = ([Subst α], Integer)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Logic (LVar α)
(===) u v = do
            (s, c) ← get
            maybe mzero (\s' → put (s', c)) $ unify u v s
            return u

-- === Should return () actually, but Logic type tightens var type
-- and a list of substitutions. I want substitutions to be polymorphic
-- so I can remove LCell type constructor from LCons. (It's a temp hack.)

fresh ∷ Logic (LVar α)
fresh = do (_, cv) ← get
           modify $ \(s, c) → (s, c+1)
           return $ LVar cv
