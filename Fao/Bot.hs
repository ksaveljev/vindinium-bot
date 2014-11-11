module Fao.Bot where

import Fao.Types

bot :: Bot
bot = Bot { initialize = undefined
          , nextMove = return Stay
          }
