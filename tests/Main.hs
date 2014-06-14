{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Test.HUnit hiding (Test)
import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import MicroKanren (LVar(LVar, LVal))
import MicroKanren.Monad
import MicroKanren.Mini

unifyWith ∷ Integer → Integer → Logic (LVar Integer)
unifyWith a b = do x ← fresh
                   x === LVal a
                   LVal b === x

test_notUnifies = assertEqual "not unifies" (run $ unifyWith 5 4) []
test_unifies    = assertEqual "unifies" (run $ unifyWith 5 5) [([(LVar 0, LVal 5)], 1)]

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Unification"
           [ testCase "not unifies" test_unifies
           , testCase "unifies" test_notUnifies
           ]
        ]
