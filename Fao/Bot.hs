module Fao.Bot where

import Fao.Goal
import Fao.Types

bot :: Bot
bot = Bot { initialize = return ()
          , nextMove = findBestGoal
          }
  where
    findBestGoal = do
      goals <- getGoals
      undefined
