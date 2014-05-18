module MicroKanren.Mini where

import MicroKanren.Core

fresh :: a
fresh = undefined

conde :: a
conde = undefined

run :: Int -> [a] -> [a]
run = take

runAll :: [a] -> [a]
runAll = id
