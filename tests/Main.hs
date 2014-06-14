{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Test.HUnit hiding (Test)
import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import MicroKanren (LVar(LVar, LVal))
import MicroKanren.Monad
import MicroKanren.Mini

unifyWith ∷ Integer → Integer → Logic (LVar Integer)
unifyWith a b = do q ← fresh
                   q === LVal a
                   LVal b === q

test_unify    = [([(LVar 0, LVal 5)], 1)] @=? (run $ unifyWith 5 5)
test_notUnify = [] @=? (run $ unifyWith 5 4)

test_reify    = [LVal 5, LVal 6] @=?
                (reify $ run' $ do q ← fresh
                                   conde [q === LVal 5
                                         ,q === (LVal 6 ∷ LVar Integer)])

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Unification"
          [ testCase "not unifies" test_notUnify
          , testCase "unifies"     test_unify
          ]
        , testGroup "Reification"
          [ testCase "reify" test_reify
          ]
        ]
