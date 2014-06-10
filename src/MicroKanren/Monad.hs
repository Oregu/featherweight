module MicroKanren.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

import MicroKanren (LVar(LVar), CanUnify, unify)

newtype L α = L { unL ∷ [α] } deriving (Show, Functor, Applicative, Alternative)
type Logic α = StateT (SC α) L α

--data Logic α = Logic { getSC ∷ [SC α], getVal ∷ α } deriving (Eq, Show)

type Subst α = (α, α)
type SC α    = ([Subst α], Integer)

instance Monad L where
  L s >>= g = L $ concat $ map (unL . g) s
  return sc = L [sc]

instance MonadPlus L where
  mzero = L []
  mplus (L s1) (L s2) = L $ if null s1 then s2 else head s1 : mplus s2 (tail s1)

--instance Monad Logic where
--  Logic s _ >>= g = Logic $ concat $ map (getSC . g) s
--  return sc = Logic ([sc], fresh)

--instance MonadPlus Logic where
--  mzero = Logic []
--  mplus (Logic s1) (Logic s2) = Logic $ if null (getSC s1) then s2 else head s1 : mplus s2 (tail s1)

(===) ∷ (Eq α, CanUnify α) ⇒ LVar α → LVar α → Logic (LVar α)
(===) u v = do
    sc <- get
    maybe mzero (\s → return (s, snd sc)) $ unify u v (fst sc)
    return (LVar 1)

fresh ∷ Logic (LVar α)
fresh = do (_, cv) ← get
           modify $ \(s, c) → (s, c+1)
           return $ LVar cv
