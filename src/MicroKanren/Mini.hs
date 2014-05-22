module MicroKanren.Mini where

--import MicroKanren

fresh ∷ a
fresh = undefined

conde ∷ a
conde = undefined

run ∷ Int -> [a] -> [a]
run = take

runAll ∷ [a] -> [a]
runAll = id
