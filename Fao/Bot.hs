module Fao.Bot where

import Data.List (sortBy, intercalate)
import Data.Maybe (fromJust)
import Control.Monad.State (get)
import System.Log.FastLogger
import Control.Monad.IO.Class (liftIO)

import Fao.Pathfinding
import Fao.Goal
import Fao.Types
import Fao.Utils

bot :: Bot
bot = Bot { initialize = return ()
          , nextMove = findBestGoal
          }
  where
    findBestGoal = do
      (BotState state _) <- get
      goals <- getGoals
      scores <- mapM goalScore goals
      dist <- mapM goalDistance goals
      let ourHero = vindiniumHero state
          goalStats = zip3 goals scores dist
          goalValue (_, v1, d1) (_, v2, d2) = case compare v1 v2 of
                                                EQ -> compare d1 d2
                                                LT -> GT
                                                GT -> LT
          bestGoals = take 50 $ sortBy goalValue goalStats
      bestAvailableGoal <- findM reachableGoal bestGoals
      liftIO $ pushLogStr globalLogger $ 
        toLogStr ("Turn number: " ++ show ((gameTurn . vindiniumGame) state) ++ "\n" ++
                  "Our hero: health = " ++ show (heroLife ourHero) ++ "\n" ++
                  "Best goals: " ++ intercalate ", " (map showGoal bestGoals) ++ "\n" ++
                  "Best available goal: " ++ maybe "" showGoal bestAvailableGoal ++ "\n"
                 )
      case bestAvailableGoal of
        Nothing -> return Stay
        (Just (Goal action pos, s, _)) -> do
          hbm <- ourHeroBoardMap action
          return $ if s > 0
                     then walk ourHero (fromJust $ hbm pos)
                     else Stay
