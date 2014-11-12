module Fao.Bot where

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Control.Monad.State (get)

import Fao.Pathfinding
import Fao.Goal
import Fao.Types
import Fao.Utils

--import Debug.Trace

bot :: Bot
bot = Bot { initialize = return ()
          , nextMove = findBestGoal
          }
  where
    findBestGoal = do
      (BotState state _ _) <- get
      goals <- getGoals
      scores <- mapM goalScore goals
      dist <- mapM goalDistance goals
      let ourHero = vindiniumHero state
          goalStats = zip3 goals scores dist
          goalValue (_, v1, d1) (_, v2, d2) = case compare v1 v2 of
                                                EQ -> compare d1 d2
                                                LT -> GT
                                                GT -> LT
          bestGoals = take 5 $ sortBy goalValue goalStats
          reachableGoal g@(Goal action pos, _, _) = do
            hbm <- heroBoardMap action
            let path = hbm pos
            case path of
              Nothing -> return Nothing
              _ -> return $ Just g
      bestAvailableGoal <- findM reachableGoal bestGoals
      {-
      trace ("Turn number: " ++ show ((gameTurn . vindiniumGame) state) ++ "\n" ++
                          "Our hero: health = " ++ show (heroLife ourHero) ++ "\n" ++
                          "Best goals: " ++ show bestGoals ++ "\n" ++
                          "Best available goal: " ++ show bestAvailableGoal ++ "\n"
                         ) $ return ()
                         -}
      case bestAvailableGoal of
        Nothing -> return Stay
        (Just (Goal action pos, s, _)) -> do
          hbm <- heroBoardMap action
          return $ if s > 0
                     then walk ourHero (fromJust $ hbm pos)
                     else Stay
