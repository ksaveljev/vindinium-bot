module Fao.Goal ( getGoals
                , goalScore
                , goalDistance
                , heroBoardMap
                , Goal (..)
                ) where

import Data.Maybe (fromJust)
import Control.Monad.State (get)
import qualified Data.Map as M

import Fao.Pathfinding
import Fao.Types
import Fao.Utils

data Action = Heal | Kill Hero | CaptureMine deriving (Show)

data Goal = Goal Action Pos deriving (Show)

heroBoardMap :: Action -> Fao HeroBoardMap
heroBoardMap action = do
    (BotState state bm sbm) <- get
    let ourHero = vindiniumHero state
        hbm = case action of
                (Kill _) -> fromJust $ M.lookup (heroId ourHero) bm
                _ -> fromJust $ M.lookup (heroId ourHero) sbm
    return hbm

canKill :: Hero -> Hero -> Int -> Bool
canKill assassin victim dist =
    let assassinLife = fromIntegral $ heroLife assassin
        victimLife = fromIntegral $ heroLife victim
    in assassinLife - dist > victimLife + 20

needToHeal :: Hero -> Int -> Bool
needToHeal hero enemyDistance = if enemyDistance > 1
                                  then heroLife hero < 21
                                  else True

loseEverything :: Hero -> Int
loseEverything hero = negate (10 * fromIntegral (heroMineCount hero))

tooMuchHealth :: Hero -> Bool
tooMuchHealth hero = heroLife hero > 90

canCaptureMine :: Hero -> Int -> Bool
canCaptureMine ourHero dist = fromIntegral (heroLife ourHero) - dist > 20

nearestEnemy :: Fao Int
nearestEnemy = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap (Kill undefined)
    let enemies = getEnemies state
    return $ minimum $ map (\enemy -> maybe 9999 distance (hbm $ heroPos enemy)) enemies

goalScore :: Goal -> Fao Int
goalScore (Goal Heal pos) = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap Heal
    enemyDistance <- nearestEnemy
    let ourHero = vindiniumHero state
        calculateScore path =
          let dist = distance path
          in case () of
               _
                | needToHeal ourHero enemyDistance -> 9999 -- negate (loseEverything ourHero)
                | tooMuchHealth ourHero -> loseEverything ourHero
                | dist > 1 -> dist
                | otherwise -> if heroLife ourHero < 81 then 9999 else dist * (-20)
    return $ maybe (-9999) calculateScore (hbm pos)

goalScore (Goal CaptureMine pos) = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap CaptureMine
    enemyDistance <- nearestEnemy
    let ourHero = vindiniumHero state
        calculateScore path =
          let dist = distance path
          in if canCaptureMine ourHero dist
               then if enemyDistance > 1
                      then 100
                      else 100 - (100 `div` enemyDistance)
               else loseEverything ourHero
    return $ maybe (-9999) calculateScore (hbm pos)

goalScore (Goal (Kill enemy) pos) = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap (Kill enemy)
    let ourHero = vindiniumHero state
        calculateScore path =
          let dist = distance path
          in if dist < 7 && canKill ourHero enemy dist
               then negate (loseEverything enemy)
               else loseEverything ourHero
    return $ maybe (-9999) calculateScore (hbm pos)

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

goalDistance :: Goal -> Fao Int
goalDistance (Goal action pos) = do
    hbm <- heroBoardMap action
    let path = hbm pos
    return $ maybe 9999 distance path
