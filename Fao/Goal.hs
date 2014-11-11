module Fao.Goal ( getGoals
                , goalScore
                ) where

import Control.Monad.State (get)

import Fao.Pathfinding
import Fao.Types
import Fao.Utils

data Action = Heal | Kill Hero | CaptureMine deriving (Show)

data Goal = Goal Action Pos deriving (Show)

goalScore :: Goal -> Fao Int
goalScore goal = do
    (BotState state hbm hsbm) <- get
    undefined

getGoals :: Fao [Goal]
getGoals = do
    (BotState state _ _) <- get
    let enemies = getEnemies state
        attackableMines = getMines state
        allTaverns = getTaverns state
    return $ concat [ map (\enemy -> Goal (Kill enemy) (heroPos enemy)) enemies
                    , map (Goal CaptureMine) attackableMines
                    , map (Goal Heal) allTaverns
                    ]

-- get all mines except the ones we own
getMines :: Vindinium -> [Pos]
getMines s = let hero = vindiniumHero s
                 board = tilePosition $ gameBoard $ vindiniumGame s
                 attackableMineTiles (MineTile Nothing) = True
                 attackableMineTiles (MineTile (Just heroId')) = heroId' /= heroId hero
                 attackableMineTiles _ = False
             in map snd $ filter (attackableMineTiles . fst) board

-- get all taverns
getTaverns :: Vindinium -> [Pos]
getTaverns s = taverns $ gameBoard $ vindiniumGame s

-- if there is currently no path to desired destination
-- then return max possible distance otherwise just return
-- the actual distance to the given goal position
goalDistance :: HeroBoardMap -> Goal -> Int
goalDistance heroBoardMap (Goal _ pos) =
    let path = heroBoardMap pos
    in maybe 9999 distance path
