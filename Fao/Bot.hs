module Fao.Bot where

import Control.Monad.State (get)

import Fao.Goal
import Fao.Types

bot :: Bot
bot = Bot { initialize = return ()
          , nextMove = findBestGoal
          }
  where
    findBestGoal = do
      (BotState state _ _) <- get
      goals <- getGoals state
      undefined
