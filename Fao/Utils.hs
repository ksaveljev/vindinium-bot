module Fao.Utils where

import Data.List (deleteBy)
import Data.Function (on)
import Control.Monad (msum)
import Control.Monad.Trans.Maybe
import qualified Data.Set as S

import Fao.Types

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = x * boardSize b + y

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tilePosition :: Board -> [(Tile, Pos)]
tilePosition (Board size tiles _ _) = zip tiles [Pos x y | x <- [0..size-1], y <- [0..size-1]]

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
    S.fromList $ foldl (\ps p -> if inBoard board p then p:ps else ps) [] [Pos x $ y+1, Pos x $ y-1, Pos (x+1) y, Pos (x-1) y]

-- get all heroes except our own one
getEnemies :: Vindinium -> [Hero]
getEnemies s = let hero = vindiniumHero s
               in deleteBy ((==) `on` heroId) hero (gameHeroes $ vindiniumGame s)

isEnemyNearby :: Vindinium -> Pos -> Bool
isEnemyNearby state pos =
    let board = gameBoard $ vindiniumGame state
        enemies = getEnemies state
    in pos `S.member` S.unions (map (adjacentTiles board . heroPos) enemies)
