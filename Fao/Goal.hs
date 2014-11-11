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
                (Kill _) -> fromJust $ M.lookup ourHero bm
                _ -> fromJust $ M.lookup ourHero sbm
    return hbm

canKill :: Hero -> Hero -> Int -> Bool
canKill assassin victim dist =
    let assassinLife = fromIntegral $ heroLife assassin
        victimLife = fromIntegral $ heroLife victim
    in assassinLife - dist > victimLife + 20

needToHeal :: Hero -> Int -> Bool
needToHeal hero dist = fromIntegral (heroLife hero) <= max (90 - dist * 5) 10

loseEverything :: Hero -> Int
loseEverything hero = negate (10 * fromIntegral (heroMineCount hero))

tooMuchHealth :: Hero -> Bool
tooMuchHealth hero = heroLife hero > 90

canCaptureMine :: Hero -> Int -> Bool
canCaptureMine ourHero dist = fromIntegral (heroLife ourHero) - dist > 20

goalScore :: Goal -> Fao Int
goalScore (Goal Heal pos) = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap Heal
    let ourHero = vindiniumHero state
        calculateScore path =
          let dist = distance path
          in case () of
               _
                | needToHeal ourHero dist -> 9999 -- negate (loseEverything ourHero)
                | tooMuchHealth ourHero -> loseEverything ourHero
                | dist > 1 -> dist
                | otherwise -> dist * (-20)
    return $ maybe (-9999) calculateScore (hbm pos)

goalScore (Goal CaptureMine pos) = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap CaptureMine
    let ourHero = vindiniumHero state
        calculateScore path =
          let dist = distance path
          in if canCaptureMine ourHero dist
               then 100
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
