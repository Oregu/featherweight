module MicroKanren.Core where

import Data.Maybe (isJust, fromJust)
import Data.List  (find)

data Var = Var Integer | Val String deriving (Eq, Show)

walk :: Var -> [(Var, Var)] -> Var
walk u@(Var _) s = let pr = find (\v -> u == fst v) s in
  if isJust pr then walk (snd $ fromJust pr) s else u
walk u _ = u

extS :: forall t t1. t -> t1 -> [(t, t1)] -> [(t, t1)]
extS x v = (:) (x, v)

(===) :: Var -> Var -> ([(Var, Var)], t) -> [([(Var, Var)], t)]
(===) u v sc = let s = unify u v (fst sc) in
  if isJust s then unit (fromJust s, snd sc) else mzero

mzero :: [a]
mzero = []

unit :: a -> [a]
unit  = flip (:) mzero

unify :: Var -> Var -> [(Var, Var)] -> Maybe [(Var, Var)]
unify u v s =
  let u' = walk u s
      v' = walk v s
      pat u2@(Var _) v2@(Var _) s2
        | u2 == v2    = Just s2
        | otherwise = Just $ extS u2 v2 s2
      pat u2@(Var _) v2 s2 = Just $ extS u2 v2 s2
      pat u2 v2@(Var _) s2 = Just $ extS v2 u2 s2
      pat u2 v2 s2
        | u2 == v2    = Just s2
        | otherwise = Nothing
   in pat u' v' s

callFresh :: (Var -> (a, Integer) -> b) -> (a, Integer) -> b
callFresh f sc = let c = snd sc in f (Var c) (fst sc, c+1)

disj :: (a -> [b]) -> (a -> [b]) -> a -> [b]
disj g1 g2 sc = mplus (g1 sc) (g2 sc)

conj :: (a -> [b]) -> (b -> [c]) -> a -> [c]
conj g1 g2 sc = bind  (g1 sc)  g2

mplus :: [a] -> [a] -> [a]
mplus s1 s2 = if null s1 then s2 else head s1 : mplus s2 (tail s1)

bind :: [a] -> (a -> [b]) -> [b]
bind s g = if null s then mzero else mplus (g $ head s) (bind (tail s) g)
