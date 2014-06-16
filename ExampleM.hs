module MicroKanren.ExampleMonadic where

import Control.Monad
import Control.Monad.Trans.State

import MicroKanren (LVar(LVal), LCons(LCons, LCell, Nil))
import MicroKanren.Monad
import MicroKanren.Mini

aANDb ∷ Logic (LVar Integer)
aANDb = do
        a ← fresh
        b ← fresh
        a === LVal 7
        mplus (b === LVal 5)
              (b === LVal 6)

runEx1, runEx2, runEx3 ∷ [LVar Integer]
runEx1 = run $ fresh >>= (=== LVal (5 ∷ Integer))
runEx2 = run aANDb
runEx3 = run $ do
               q ← fresh
               q === LVal 5
               LVal 6 === q

fives, sixes ∷ LVar Integer → Logic (LVar Integer)
fives x = mplus (x === LVal 5) (fives x)
sixes x = mplus (x === LVal 6) (sixes x)

runFives, run5and6 ∷ [LVar Integer]
runFives = run $ fresh >>= fives
run5and6 = run $ do x ← fresh
                    fives x `mplus` sixes x

appendo ∷ Eq α ⇒ LVar (LCons α) → LVar (LCons α) → LVar (LCons α) → Logic (LVar (LCons α))
appendo l s out = mplus
    (do l === LVal Nil
        s === out)
    (do h ← fresh
        t ← fresh
        l === LVal (LCons h t)
        res ← fresh
        out === LVal (LCons h res)
        appendo t s res)

runAppendo ∷ [LVar (LCons Integer)]
runAppendo = run $ do
                   q ← fresh
                   appendo q q (LVal (LCons (LVal (LCell 1)) (LVal (LCons (LVal (LCell 1)) (LVal Nil)))))

--membero ∷ Eq α ⇒ LVar (LCons α) → LVar (LCons α) → Logic (LVar (LCons α))
--membero x l = conde
--    [do t ← fresh
--        x === LVal (LCons x t)
--        return t
--    ,do t ← fresh
--        h ← fresh
--        l === LVal (LCons h t)
--        membero x t]
