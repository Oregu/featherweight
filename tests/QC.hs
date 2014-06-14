module Main (main) where

import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((===))

import MicroKanren (LVar(LVal))
import MicroKanren.Monad
import MicroKanren.Mini

unifyWith ∷ Integer → Integer → Logic (LVar Integer)
unifyWith a b = do x ← fresh
                   x === LVal a
                   LVal b === x

prop_notUnifies, prop_unifies ∷ Integer → Integer → Property
prop_notUnifies a b = a /= b ==> null $ run $ unifyWith a b
prop_unifies    a b = a == b ==> not $ null $ run $ unifyWith a b

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [
    testGroup "Unification" [
       testProperty "Fails"    $ prop_notUnifies
      ,testProperty "Succeeds" $ prop_unifies
      ]
    ]
