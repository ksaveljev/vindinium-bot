module Faorien.Play ( playTraining
                    , playArena
                    ) where

import Control.Lens ((^.), (.=))

import Faorien.Types
import Faorien.Api

playTraining :: Maybe Int -> Maybe Board -> Bot -> Faorien Activity
playTraining mt mb = playMain $ startTraining mt mb

playArena :: Bot -> Faorien Activity
playArena = playMain startArena

playMain :: Faorien Activity -> Bot -> Faorien Activity
playMain start bot = do
    activity <- start
    session .= activity
    initialize bot
    playLoop bot activity

playLoop :: Bot -> Activity -> Faorien Activity
playLoop bot activity =
    if activity^.activityGame.gameFinished
        then return activity
        else do
            newActivity <- turn bot >>= move activity
            playLoop bot newActivity
