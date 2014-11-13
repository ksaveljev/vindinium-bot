module Fao.Goal ( getGoals
                , goalScore
                , goalDistance
                , heroBoardMap
                , Goal (..)
                ) where

import Data.Maybe (fromJust)
import Control.Monad.State (get)
import qualified Data.Map as M
import qualified Data.Set as S

import Fao.Pathfinding
import Fao.Types
import Fao.Utils

data Action = Heal | Kill Hero | CaptureMine deriving (Show)

data Goal = Goal Action Pos deriving (Show)

nextToTavern :: Hero -> Fao Bool
nextToTavern hero = do
    (BotState state _ _) <- get
    let board = gameBoard $ vindiniumGame state
        tavernPositions = getTaverns state
    return $ any (`S.member` adjacentTiles board (heroPos hero)) tavernPositions

enemyNearMine :: Pos -> Fao Bool
enemyNearMine pos = do
    (BotState state _ _) <- get
    let board = gameBoard $ vindiniumGame state
        enemies = getEnemies state
    return $ any (`S.member` adjacentTiles board pos) (map heroPos enemies)

heroBoardMap :: Action -> Fao HeroBoardMap
heroBoardMap action = do
    (BotState state bm sbm) <- get
    let ourHero = vindiniumHero state
        hbm = case action of
                (Kill _) -> fromJust $ M.lookup (heroId ourHero) bm
                _ -> fromJust $ M.lookup (heroId ourHero) sbm
    return hbm

canCaptureMine :: Hero -> Int -> Bool
canCaptureMine hero dist = heroLife hero - fromIntegral dist > 20

canKill :: Hero -> Hero -> Int -> Bool
canKill assassin victim dist =
    let d = fromIntegral dist
        assassinLife = if dist `mod` 2 == 1 then heroLife assassin - d else heroLife assassin - 20 - d
        victimLife = heroLife victim - d
    in assassinLife > victimLife

nearestEnemy :: Fao (Hero, Int)
nearestEnemy = do
    (BotState state _ _) <- get
    hbm <- heroBoardMap (Kill undefined)
    let enemies = getEnemies state
    return $ minimum $ map (\enemy -> maybe (enemy, 9999) (\path -> (enemy, distance path)) (hbm $ heroPos enemy)) enemies

needToHeal :: Hero -> Bool
needToHeal hero = heroLife hero < 21

goalScore :: Goal -> Fao Int
goalScore (Goal action pos) = do
    (BotState state _ _) <- get
    let ourHero = vindiniumHero state
    hbm <- heroBoardMap (Kill undefined)
    hsbm <- heroBoardMap Heal
    (nearestHero, distNearestHero) <- nearestEnemy
    case distNearestHero of
      1 -> do
        enemyNearTavern <- nextToTavern nearestHero
        ourHeroNearTavern <- nextToTavern ourHero
        if enemyNearTavern
          then case () of
                 _ | heroLife nearestHero <= 20 -> case action of
                                                     (Kill enemy) | enemy == nearestHero -> return 1000
                                                     _ -> return (-9999)
                   | ourHeroNearTavern -> case heroLife ourHero of
                                            x | x < 70 -> case action of
                                                            Heal -> return 1000
                                                            _ -> return (-9999)
                                            _ -> case action of
                                                   (Kill enemy) | enemy == nearestHero -> return (-9999)
                                                   (Kill enemy) -> let d = maybe 9999 distance (hbm pos)
                                                                  in if d < 3
                                                                       then if canKill ourHero enemy d
                                                                              then return $ 10 * fromIntegral (heroMineCount enemy)
                                                                              else return (-9999)
                                                                       else return (-9999)
                                                   Heal -> let d = maybe 9999 distance (hsbm pos)
                                                          in if needToHeal ourHero
                                                               then return 1000
                                                               else return d
                                                   CaptureMine -> let d = maybe 9999 distance (hsbm pos)
                                                                 in if canCaptureMine ourHero d
                                                                      then return (100 - d)
                                                                      else return (-9999)
                   | otherwise -> case action of
                                    Heal -> return 1000
                                    _ -> return (-9999)
          -- there is no tavern next to the nearest enemy
          else
            if canKill ourHero nearestHero distNearestHero
              then case action of
                     (Kill enemy) | enemy == nearestHero -> return 1000
                     _ -> return (-9999)
              else case action of
                     Heal -> return 1000
                     _ -> return (-9999)
      2 -> do
        enemyNearTavern <- nextToTavern nearestHero
        if enemyNearTavern
          then case action of
                 (Kill enemy) | enemy == nearestHero -> return (-9999)
                 (Kill enemy) -> let d = maybe 9999 distance (hbm pos)
                                in if d < 3
                                     then if canKill ourHero enemy d
                                            then return $ 10 * fromIntegral (heroMineCount enemy)
                                            else return (-9999)
                                     else return (-9999)
                 Heal -> let d = maybe 9999 distance (hsbm pos)
                        in if needToHeal ourHero
                             then return 1000
                             else return d
                 CaptureMine -> let d = maybe 9999 distance (hsbm pos)
                               in if canCaptureMine ourHero d
                                    then return (100 - d)
                                    else return (-9999)
          else
            if canKill ourHero nearestHero distNearestHero
              then case action of
                     (Kill enemy) | enemy == nearestHero -> return 1000 -- need to try killing this enemy
                     _ -> return (-9999)
              else case action of
                     (Kill enemy) | enemy == nearestHero -> return (-9999)
                     (Kill enemy) -> let d = maybe 9999 distance (hbm pos)
                                    in if d < 3
                                         then if canKill ourHero enemy d
                                                then return $ 10 * fromIntegral (heroMineCount enemy)
                                                else return (-9999)
                                         else return (-9999)
                     Heal -> let d = maybe 9999 distance (hsbm pos)
                            in if needToHeal ourHero
                                 then return 1000
                                 else return d
                     CaptureMine -> let d = maybe 9999 distance (hsbm pos)
                                   in if canCaptureMine ourHero d
                                        then return (100 - d)
                                        else return (-9999)
      _ -> case action of
             (Kill _) -> return (-9999) -- not going to run towards the enemy if he is far away
             Heal -> do
               ourHeroNearTavern <- nextToTavern ourHero
               case () of
                 _ | ourHeroNearTavern && heroLife ourHero < 90 -> return 1000
                   | needToHeal ourHero -> return 1000
                   | otherwise -> let d = maybe 9999 distance (hsbm pos)
                                 in return d
             CaptureMine -> let d = maybe 9999 distance (hsbm pos)
                           in do
                             mineProtected <- enemyNearMine pos
                             if mineProtected
                               then return 50
                               else if canCaptureMine ourHero d
                                      then return 100
                                      else return (-9999)

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
