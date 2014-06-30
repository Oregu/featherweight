module MicroKanren.ExampleMonadic where

import Control.Monad
import Control.Monad.Trans.State

import MicroKanren
import MicroKanren.Plain (LVar(..), LCons(..), CanUnify)

aANDb ∷ Logic (LVar Int)
aANDb = do
        a ← fresh
        b ← fresh
        a === LVal 7
        mplus (b === LVal 5)
              (b === LVal 6)

runEx1, runEx2, runEx3 ∷ [LVar Int]
runEx1 = run $ fresh >>= (=== LVal 5)
runEx2 = run aANDb
runEx3 = run $ do
               q ← fresh
               q === LVal 5
               LVal 6 === q

fives, sixes ∷ LVar Int → Logic (LVar Int)
fives x = mplus (x === LVal 5) (fives x)
sixes x = mplus (x === LVal 6) (sixes x)

runFives, run5and6 ∷ [LVar Int]
runFives = run $ fresh >>= fives
run5and6 = run $ do x ← fresh
                    fives x `mplus` sixes x

appendo ∷ (CanUnify α, Eq α) ⇒ LVar (LCons α) → LVar (LCons α) → LVar (LCons α) → Logic (LVar (LCons α))
appendo l s out = mplus
    (do l === LVal Nil
        s === out)
    (do h ← fresh
        t ← fresh
        l === LVal (LCons h t)
        res ← fresh
        out === LVal (LCons h res)
        appendo t s res)

runAppendo ∷ [LVar (LCons Int)]
runAppendo = run $ do
                   q ← fresh
                   appendo q q (LVal (LCons (LVal (LCell (1 ∷ Int))) (LVal (LCons (LVal (LCell (1 ∷ Int))) (LVal Nil)))))

--membero ∷ Eq α ⇒ LVar (LCons α) → LVar (LCons α) → Logic (LVar (LCons α))
--membero x l = conde
--    [do t ← fresh
--        x === LVal (LCons x t)
--        return t
--    ,do t ← fresh
--        h ← fresh
--        l === LVal (LCons h t)
--        membero x t]
