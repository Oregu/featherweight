module Main (main) where

import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List

qsort ∷ Ord a ⇒ [a] → [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent ∷ [Integer] → Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [
    testGroup "qsort" [ testProperty "Idempotent" $ prop_idempotent ]
    ]
