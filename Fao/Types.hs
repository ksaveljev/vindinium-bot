{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Fao.Types ( Fao
                 , runFao
                 , asks
                 , Settings (..)
                 , Key (..)
                 , Bot(..)
                 , BotState(..)
                 , Vindinium (..)
                 , GameId (..)
                 , Game (..)
                 , HeroId (..)
                 , Hero (..)
                 , Board (..)
                 , Tile (..)
                 , Pos (..)
                 , Dir (..)
                 , Distance
                 , HeroBoardMap
                 , Path (..)
                 , BoardMap
                 ) where

import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Aeson
import Data.Monoid ((<>))
import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M

newtype Key = Key Text deriving (Show, Eq)

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

newtype Fao a = Fao { unFao :: StateT BotState (ReaderT Settings IO) a }
    deriving (Functor, Applicative, Monad, MonadReader Settings, MonadState BotState, MonadIO)

runFao :: Settings -> BotState -> Fao a -> IO a
runFao s st = flip runReaderT s . flip evalStateT st . unFao

data Bot = Bot { initialize :: Fao ()
               , nextMove :: Fao Dir
               }

data BotState = BotState { vindinium :: Vindinium
                         , boardMap :: BoardMap
                         , safeBoardMap :: BoardMap
                         }

newtype Path  = Path [Pos] deriving (Show, Eq)

type HeroBoardMap = Pos -> Maybe Path

type BoardMap = M.Map Hero HeroBoardMap

data Vindinium = Vindinium { vindiniumGame    :: Game
                           , vindiniumHero    :: Hero
                           , vindiniumToken   :: Text
                           , vindiniumViewUrl :: Text
                           , vindiniumPlayUrl :: Text
                           } deriving (Show, Eq)

newtype GameId = GameId Text deriving (Show, Eq)

data Game = Game { gameId       :: GameId
                 , gameTurn     :: Integer
                 , gameMaxTurns :: Integer
                 , gameHeroes   :: [Hero]
                 , gameBoard    :: Board
                 , gameFinished :: Bool
                 } deriving (Show, Eq)

newtype HeroId = HeroId Int deriving (Show, Eq, Ord)

data Hero = Hero { heroId        :: HeroId
                 , heroName      :: Text
                 , heroUserId    :: Maybe Text
                 , heroElo       :: Maybe Integer
                 , heroPos       :: Pos
                 , heroLife      :: Integer
                 , heroGold      :: Integer
                 , heroMineCount :: Integer
                 , heroSpawnPos  :: Pos
                 , heroCrashed   :: Bool
                 } deriving (Show, Eq, Ord)

data Board = Board { boardSize  :: Int
                   , boardTiles :: [Tile]
                   , mines :: [Pos]
                   , taverns :: [Pos]
                   } deriving (Show, Eq)

data Tile = FreeTile
          | WoodTile
          | TavernTile
          | HeroTile HeroId
          | MineTile (Maybe HeroId)
    deriving (Show, Eq)

data Pos = Pos { posX :: Int
               , posY :: Int
               } deriving (Show, Eq, Ord)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq)

type Distance = Pos -> Pos -> Int -- Distance function between two positions on a board

instance ToJSON Key where
    toJSON (Key k) = String k

instance ToJSON Board where
    toJSON b  = object [ "size"  .= boardSize b
                       , "tiles" .= printTiles (boardTiles b)
                       ]

instance FromJSON Vindinium where
    parseJSON (Object o) = Vindinium <$> o .: "game"
                                     <*> o .: "hero"
                                     <*> o .: "token"
                                     <*> o .: "viewUrl"
                                     <*> o .: "playUrl"
    parseJSON _ = mzero

instance FromJSON Game where
    parseJSON (Object o) = Game <$> o .: "id"
                                <*> o .: "turn"
                                <*> o .: "maxTurns"
                                <*> o .: "heroes"
                                <*> o .: "board"
                                <*> o .: "finished"
    parseJSON _ = mzero

instance FromJSON GameId where
    parseJSON x = GameId <$> parseJSON x

instance FromJSON Hero where
    parseJSON (Object o) = Hero <$> o .: "id"
                                <*> o .: "name"
                                <*> o .:? "userId"
                                <*> o .:? "elo"
                                <*> o .: "pos"
                                <*> o .: "life"
                                <*> o .: "gold"
                                <*> o .: "mineCount"
                                <*> o .: "spawnPos"
                                <*> o .: "crashed"
    parseJSON _ = mzero

instance FromJSON HeroId where
    parseJSON x = HeroId <$> parseJSON x

instance FromJSON Pos where
    parseJSON (Object o) = Pos <$> o .: "x" <*> o .: "y"
    parseJSON _ = mzero

instance FromJSON Board where
    parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

instance ToJSON Dir where
    toJSON Stay = String "Stay"
    toJSON North = String "North"
    toJSON South = String "South"
    toJSON East = String "East"
    toJSON West = String "West"

parseBoard :: Int -> String -> Board
parseBoard s txt =
    --Board s $ map parse (chunks t)
    let xToPos x sz = uncurry Pos pr
                      where pr = x `divMod` sz
        (tls, mns, tvs, _) = foldl' (\(ts,mm,mt,pos) ch ->
                                      let t = parse ch
                                      in case t of
                                          TavernTile -> (t:ts,mm,xToPos pos s:mt,pos+1)
                                          MineTile _ -> (t:ts, xToPos pos s:mm,mt,pos+1)
                                          _          -> (t:ts, mm, mt, pos+1)
                                    ) ([],[],[],0) (chunks txt)
    in Board s (reverse tls) mns tvs
  where
    chunks []       = []
    chunks [_]      = error "chunks: even chars number"
    chunks (a:b:xs) = (a, b):chunks xs

    parse (' ', ' ') = FreeTile
    parse ('#', '#') = WoodTile
    parse ('@', x)   = HeroTile $ HeroId $ read [x]
    parse ('[', ']') = TavernTile
    parse ('$', '-') = MineTile Nothing
    parse ('$', x)   = MineTile $ Just $ HeroId $ read [x]
    parse (a, b)     = error $ "parse: unknown tile pattern " ++ show [a,b]

printTiles :: [Tile] -> Text
printTiles =
    foldl (<>) "" . map printTile
  where
    printTile FreeTile = "  "
    printTile WoodTile = "##"
    printTile (HeroTile (HeroId i)) = "@" <> pack (show i)
    printTile TavernTile = "[]"
    printTile (MineTile Nothing) = "$-"
    printTile (MineTile (Just (HeroId i))) = "$" <> pack (show i)
