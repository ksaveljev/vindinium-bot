module Faorien.Utils where

import Data.List (deleteBy, foldl')
import Data.Function (on)
import Control.Lens ((^.))
import Control.Monad (msum)
import Control.Monad.Trans.Maybe
import System.Log.FastLogger
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as S

import Faorien.Types

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ (b^.boardTiles) !! idx
        else Nothing
  where
    idx = x * b^.boardSize + y

{-
isMineTileAt :: Board -> Pos -> Bool
isMineTileAt board pos =
    case tileAt board pos of
      Just (MineTile _) -> True
      _ -> False
      -}

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = b^.boardSize
    in x >= 0 && x < s && y >= 0 && y < s

tilePosition :: Board -> [(Tile, Pos)]
tilePosition (Board size tiles) = zip tiles [Pos x y | x <- [0..size-1], y <- [0..size-1]]

-- http://en.wikipedia.org/wiki/Taxicab_geometry
manhattan :: Distance
manhattan (Pos row1 col1) (Pos row2 col2) = abs (row1 - row2) + abs (col1 - col2)

dirFromPos :: Pos -> Pos -> Dir
dirFromPos (Pos fx fy) (Pos tx ty) =
    let x = tx - fx
        y = ty - fy
    in case (x, y) of
         (-1, 0) -> North
         (1, 0) -> South
         (0, -1) -> West
         (0, 1) -> East
         _ -> Stay

adjacentTiles :: Board -> Pos -> S.Set Pos
adjacentTiles board (Pos x y) =
    S.fromList $ foldl' (\ps p -> if inBoard board p then p:ps else ps) [] [Pos x $ y+1, Pos x $ y-1, Pos (x+1) y, Pos (x-1) y]

-- get all heroes except our own one
getEnemies :: Activity -> [Hero]
getEnemies s = let hero = s^.activityHero
               in deleteBy ((==) `on` flip (^.) heroId) hero (s^.activityGame.gameHeroes)

-- TODO: there is a problem with this function if we have this kind of map:
-- EWF
-- WFF
-- FHF
-- where E is enemy, W is wood, F is free and H is hero then this method will
-- not allow our hero to go to the middle tile because it thinks it is adjacent
-- to that middle tile. But actually it is safe to go there as our enemy needs
-- more than 2 moves to actually reach this middle tile.
isEnemyNearby :: Activity -> Pos -> Bool
isEnemyNearby activity pos =
    let board = activity^.activityGame.gameBoard
        enemies = getEnemies activity
    in pos `S.member` S.unions (map (adjacentTiles board . flip (^.) heroPos) enemies)

{-# NOINLINE globalLogger #-}
globalLogger :: LoggerSet
globalLogger = unsafePerformIO $ newFileLoggerSet defaultBufSize "./debug.log"
