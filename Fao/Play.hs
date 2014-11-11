module Fao.Play ( playTraining
                , playArena
                ) where

import Fao.Types
import Fao.Api

playTraining :: Maybe Int -> Maybe Board -> Bot -> Fao Vindinium
playTraining mt mb b = startTraining mt mb >>= playLoop b

playArena :: Bot -> Fao Vindinium
playArena b = startArena >>= playLoop b

playLoop :: Bot -> Vindinium -> Fao Vindinium
playLoop bot state =
    if (gameFinished . vindiniumGame) state
        then return state
        else do
            newState <- nextMove bot >>= move state
            playLoop bot newState
