module Fao.Goal ( getGoals
                , goalScore
                , goalDistance
                , ourHeroBoardMap
                , Goal (..)
                , showGoal
                , reachableGoal
                ) where

import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import Control.Monad.State (get)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Fao.Pathfinding
import Fao.Types
import Fao.Utils

data Action = Heal | Kill Hero | CaptureMine deriving (Show)

data Goal = Goal Action Pos deriving (Show)

-- used for logging
showGoal :: (Goal, Int, Int) -> String
showGoal (Goal CaptureMine _, score, dist) = "CaptureMine (" ++ show score ++ ") [" ++ show dist ++ "]"
showGoal (Goal Heal _, score, dist) = "Heal (" ++ show score ++ ") [" ++ show dist ++ "]"
showGoal (Goal (Kill enemy) _, score, dist) = "Kill " ++ T.unpack (heroName enemy) ++ " life = " ++ show (heroLife enemy) ++ " (" ++ show score ++ ") [" ++ show dist ++ "]"

-- find out if there is someone near a tavern (it might be our hero or an
-- enemy), this information is useful for us to make correct decisions in
-- different situations
-- one of those situations is we would like to avoid taverns which has an
-- enemy near it as we are moving there to heal and probably have low
-- health so that enemy might intercept us and kill us easily so we better
-- choose some other tavern
nextToTavern :: Hero -> Fao Bool
nextToTavern hero = do
    (BotState state _) <- get
    let board = gameBoard $ vindiniumGame state
        tavernPositions = getTaverns state
    return $ any (`S.member` adjacentTiles board (heroPos hero)) tavernPositions

-- find out if there is an enemy somewhere near the mine as we do not want
-- to try and conquer that particular mine due to the high risk of fighting
-- (and losing) or simply wasting time on taking and retaking the mine
-- until the health drops (TODO: this might actually be bad for small maps
-- with small amount of mines)
enemyNearMine :: Pos -> Fao Bool
enemyNearMine pos = do
    (BotState state _) <- get
    let board = gameBoard $ vindiniumGame state
        enemies = getEnemies state
    return $ any (`S.member` adjacentTiles board pos) (map heroPos enemies)

-- get the shortest path map for our hero based on the action we are taking
ourHeroBoardMap :: Action -> Fao HeroBoardMap
ourHeroBoardMap action = do
    (BotState state _) <- get
    let ourHero = vindiniumHero state
    heroBoardMap ourHero action

-- looks up a shortest path map for a particular hero (might be our hero or an enemy)
-- for Kill action the shortest path map has direct route
-- for other actions the shortest path map avoids other heroes in its pathing
heroBoardMap :: Hero -> Action -> Fao HeroBoardMap
heroBoardMap hero action = do
    (BotState _ (Internal bm sbm)) <- get
    let hbm = case action of
                (Kill _) -> fromJust $ M.lookup (heroId hero) bm
                _ -> fromJust $ M.lookup (heroId hero) sbm
    return hbm

canCaptureMine :: Hero -> Int -> Bool
canCaptureMine hero dist = heroLife hero - fromIntegral dist > 20

-- we try to decide if we can kill an enemy based on the distance between us
-- if the distance is somewhat small (we are next to an enemy or there is
-- only one square between us and a move will result in an attack) then we
-- do not have any penalty on our health but if the distance is larger than
-- that then we need to calculate who can start the fight first and if the
-- enemy is the first to attack then we must have a buffer of 20 health to
-- be able to win that fight
canKill :: Hero -> Hero -> Int -> Bool
canKill assassin victim dist =
    let d = fromIntegral dist
        assassinLife = if dist < 3 || dist `mod` 2 == 1 then heroLife assassin - d else heroLife assassin - 20 - d
        victimLife = heroLife victim - d
    in assassinLife > victimLife

-- find nearest enemy to our hero, returns only one hero even though there
-- are situations when several enemies are at the same distance from us
nearestEnemy :: Fao (Hero, Int)
nearestEnemy = do
    (BotState state _) <- get
    let ourHero = vindiniumHero state
    nearestEnemyTo (heroPos ourHero)

-- find nearest enemy to some position on the map
-- this is used to find nearest enemy to our hero, nearest enemy to the
-- tavern we are moving to and so on
nearestEnemyTo :: Pos -> Fao (Hero, Int)
nearestEnemyTo pos = do
    (BotState state _) <- get
    let enemies = getEnemies state
        enemyToDistance enemy = do
          hbm <- heroBoardMap enemy (Kill undefined)
          return $ maybe (enemy, 9999) (\path -> (enemy, distance path)) (hbm pos)
    enemyDistances <- mapM enemyToDistance enemies
    return $ minimumBy (compare `on` snd) enemyDistances

-- basically Heal is dominating command when we are below 21 health because
-- we cannot do much (cannot attack mines and fighting is pretty hard) but
-- there may be situations when killing an enemy might take over the
-- healing goal (when the enemy is one square away and has less that 21
-- health as well, as we will attack him but will not lose any health)
needToHeal :: Hero -> Bool
needToHeal hero = heroLife hero < 21

-- after we have scored all the goals and sorted them by their value and
-- distance we want to find the first goal in that sorted list which is
-- actually reachable for our hero (see Bot.hs for soring and selecting the
-- best reachable goal)
reachableGoal :: (Goal, Int, Int) -> Fao (Maybe (Goal, Int, Int))
reachableGoal goal@(Goal action pos, _, dist) = do
    (BotState state _) <- get
    -- we grab a map which uses direct shortest path if the action is Kill
    -- and the safe shortest path (avoiding enemies) when the action is
    -- something other than Kill
    hbm <- ourHeroBoardMap action
    let ourHero = vindiniumHero state
        path = hbm pos
    -- first thing we check if we actually can reach the goal position
    -- using our shortest path HeroBoardMap
    case path of
      Nothing -> return Nothing
      -- ok, so the path is reachable but we need to do more thinking to
      -- identify if we should actually follow this path
      Just (Path p) -> do
        -- grab the next position our hero will end up in if we follow this path
        let nextPos = head p
        -- we find the nearest enemy to this new position we will end up in
        (nearestHero, distNearestHero) <- nearestEnemyTo nextPos
        -- and think differently depending on the goal we are about to reach
        case action of
          Heal -> do
            ourHeroNearTavern <- nextToTavern ourHero
            if ourHeroNearTavern
              -- if our hero is near the tavern then we do not need to
              -- think and proceed with healing
              then return $ Just goal
              -- but if we are not near the tavern but the goal is Heal (so
              -- we want to move towards the tavern) we need to see if
              -- there is a nearby enemy that can cause us trouble
              --
              -- we look at special case when there are 2 empty squares
              -- between our hero and the enemy, and we take a step towards
              -- that enemy, which means that if we take this path the enemy 
              -- may attack us on his next move if he wishes and we do not 
              -- want to end up in a losing situation
              else if distNearestHero == 2
                     then if canKill ourHero nearestHero distNearestHero
                            then return $ Just goal
                            else return Nothing
                     -- in all other situations we want to have a look if
                     -- the tavern we are moving to has any enemies near it
                     -- cause if there is someone they might kill us and we
                     -- should try selecting other tavern which is more safe
                     else do
                       (nearestTavernHero, distNearestTavernHero) <- nearestEnemyTo pos
                       if distNearestTavernHero < 3
                         -- so the enemy is near the tavern, but before dismissing this
                         -- tavern we need to check if we can reach it before the enemy
                         -- can attack us (this situation may arise when there is only
                         -- 1 square between our hero and the tavern, and the enemy is on
                         -- the other side of the tavern, so he cannot reach us in time)
                         --
                         -- TODO: we must also think about our health, we might reach the
                         -- tavern taking a few hits on our way!!! but right now it seems
                         -- we avoid going to the tavern even with enough health to take
                         -- a beating
                         then do
                           ebm <- heroBoardMap nearestTavernHero (Kill undefined)
                           let ourHeroFinalDestination = last p
                           case (ebm ourHeroFinalDestination) of
                             Nothing -> return $ Just goal
                             Just enemyPath -> if dist < distance enemyPath
                                                 then return $ Just goal
                                                 else return Nothing
                         else return $ Just goal
          -- for capture mine goal there is the same special case we want
          -- to look at: when there are two empty squares between us and
          -- the nearest enemy, and we take a step towards that enemy,
          -- which means that if we take this path the enemy may attack us
          -- on his next move if he wishes and we do not want to end up in
          -- a losing situation
          CaptureMine -> if distNearestHero == 2
                           then if canKill ourHero nearestHero distNearestHero
                                  then return $ Just goal
                                  else return Nothing
                           else return $ Just goal
          -- when we are determined to kill an enemy then there is not much
          -- thinking involved (at least at this stage)
          (Kill _) -> return $ Just goal

goalScore :: Goal -> Fao Int
goalScore (Goal action pos) = do
    (BotState state _) <- get
    let ourHero = vindiniumHero state
    hbm <- ourHeroBoardMap (Kill undefined)
    hsbm <- ourHeroBoardMap Heal
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
             Heal -> do
               ourHeroNearTavern <- nextToTavern ourHero
               case () of
                     -- heal full when we are at the tavern already and our
                     -- health is not near full
                 _ | ourHeroNearTavern && heroLife ourHero < 90 -> return 1000
                     -- our health is really low so we need to make Heal
                     -- action as our priority
                   | needToHeal ourHero -> return 1000
                     -- no particular need to heal yet so if there is
                     -- an existing path towards the selected tavern then
                     -- we simply give it a score equal to distance until
                     -- we can reach it using safe shortest path map
                   | otherwise -> let d = maybe 9999 distance (hsbm pos)
                                 in return d
             CaptureMine -> let d = maybe 9999 distance (hsbm pos)
                           in do
                             -- if there is an enemy near the mine then we would like to avoid that mine and just
                             -- concentrate on the other ones which are less risky to attack
                             mineProtected <- enemyNearMine pos
                             if mineProtected
                               -- we still give it a good score though as it will be chosen when all other goals
                               -- are not reachable (maybe we have all other mines captured! or they are unreachable)
                               then return 50
                               -- and if there is no nearby enemy then we definetly would like to attack it if we
                               -- have enough health and don't have some other goal with major priority (1000)
                               else if canCaptureMine ourHero d
                                      then return 100
                                      else return (-9999)

-- generate goals from the map
-- every single hero (enemy) is a Goal for us to Kill
-- every single mine is a Goal for us to CaptureMine
-- every single tavern is a Goal for us to Heal
getGoals :: Fao [Goal]
getGoals = do
    (BotState state _) <- get
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

-- get all taverns on the map (we precalculate it in Api.hs I think)
getTaverns :: Vindinium -> [Pos]
getTaverns s = taverns $ gameBoard $ vindiniumGame s

goalDistance :: Goal -> Fao Int
goalDistance (Goal action pos) = do
    hbm <- ourHeroBoardMap action
    let path = hbm pos
    return $ maybe 9999 distance path
