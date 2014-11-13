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
      -- we are standing next to an enemy, this means that we cannot run
      -- from this enemy as every his turn is going to result in an attack
      -- on us (if that bot decides to kill us), so we need to find a way
      -- to handle this situation
      1 -> do
        -- we get the information if the enemy is near the tavern or our
        -- hero is near the tavern, going to use this info when making
        -- decisions below
        enemyNearTavern <- nextToTavern nearestHero
        ourHeroNearTavern <- nextToTavern ourHero
        -- so, the enemy is near the tavern, pretty bad situation
        if enemyNearTavern
          then case () of
                 -- but if the enemy is low on health then we can kill it!
                 _ | heroLife nearestHero <= 20 -> case action of
                                                     -- killing is the only action we accept,
                                                     -- everything else is avoided
                                                     (Kill enemy) | enemy == nearestHero -> return 1000
                                                     _ -> return (-9999)
                 -- our hero is also near a tavern! some maps can have this
                 -- situation, so we need to somehow handle this case and
                 -- try not to stay at the tavern for the rest of the game
                   | ourHeroNearTavern -> case heroLife ourHero of
                                            -- if our health is somewhat low we must heal, nothing else
                                            x | x < 70 -> case action of
                                                            Heal -> return 1000
                                                            _ -> return (-9999)
                                            -- our health is OK, so we need to think what we can do
                                            -- knowing that the enemy is next to us
                                            _ -> case action of
                                                   -- we definetly have no chance killing the enmy who is next to a tavern
                                                   (Kill enemy) | enemy == nearestHero -> return (-9999)
                                                   -- any other enemy though can be a target for our next goal
                                                   (Kill enemy) -> let d = maybe 9999 distance (hbm pos)
                                                                  -- if the enemy is close then we could attack it (if we can kill it)
                                                                  in if d < 3
                                                                       then if canKill ourHero enemy d
                                                                              then return $ 10 * fromIntegral (heroMineCount enemy)
                                                                              else return (-9999)
                                                                       else return (-9999)
                                                   -- probably could heal (i think we never reach this branch though)
                                                   Heal -> let d = maybe 9999 distance (hsbm pos)
                                                          in if needToHeal ourHero
                                                               then return 1000
                                                               else return d
                                                   -- we could try escaping this situation by going to some mine
                                                   -- (this has to be reworked i think)
                                                   CaptureMine -> let d = maybe 9999 distance (hsbm pos)
                                                                 in if canCaptureMine ourHero d
                                                                      then return (100 - d)
                                                                      else return (-9999)
                   -- so we have an enemy near tavern who has more than 20
                   -- health and we are not next to a tavern, time to find
                   -- one! and go to it... this doesn't work well, needs to
                   -- be fixed, the pathing has to be done better in this
                   -- situation
                   | otherwise -> case action of
                                    Heal -> return 1000
                                    _ -> return (-9999)
          -- there is no tavern next to the nearest enemy so we are simply
          -- facing him in a fight
          else
            -- if we can kill him then it is our priority and nothing else matters
            if canKill ourHero nearestHero distNearestHero
              then case action of
                     (Kill enemy) | enemy == nearestHero -> return 1000
                     _ -> return (-9999)
              -- but if cannot kill him then we must TRY to escape to a tavern
              else case action of
                     Heal -> return 1000
                     _ -> return (-9999)
      -- there is a 1 move gap between our hero and the nearest enemy
      2 -> do
        -- we need to know if the enemy is standing next to a tavern
        enemyNearTavern <- nextToTavern nearestHero
        -- and if the enemy is
        if enemyNearTavern
          then case action of
                 -- then there is no point in attacking it
                 (Kill enemy) | enemy == nearestHero -> return (-9999)
                 -- the rest is just giving some score to other actions
                 -- this has to be updated as well as we need a good
                 -- pathing to avoid the nearest enemy as he can attack us
                 -- if we move in some bad direction
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
            -- enemy is not near the tavern so we can try killing him
            if canKill ourHero nearestHero distNearestHero
              then case action of
                     (Kill enemy) | enemy == nearestHero -> return 1000 -- need to try killing this enemy
                     _ -> return (-9999)
              -- but if we cannot kill him
              else case action of
                     -- then avoid that enemy
                     (Kill enemy) | enemy == nearestHero -> return (-9999)
                     -- and score other actions accordingly
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
      -- there is no enemy near us (dist > 2)
      _ -> case action of
             -- we are not going to go for any enemy (although need to
             -- update this to check the situation when dist == 3)
             (Kill _) -> return (-9999)
             -- too boring to add comments here
             Heal -> do
               ourHeroNearTavern <- nextToTavern ourHero
               case () of
                 _ | ourHeroNearTavern && heroLife ourHero < 90 -> return 1000
                   | needToHeal ourHero -> return 1000
                   | otherwise -> let d = maybe 9999 distance (hsbm pos)
                                 in return d
             -- too boring to add comments here
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
