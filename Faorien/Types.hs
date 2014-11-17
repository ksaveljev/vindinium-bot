{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Faorien.Types
        ( Vindinium
        , runVindinium
        , asks
        , Settings (..)
        , Key (..)
        , Bot
        , State (..)
        , GameId (..)
        , Game (..)
        , HeroId (..)
        , Hero (..)
        , Board (..)
        , Tile (..)
        , Pos (..)
        , Dir (..)
        )
    where

import Data.Text (Text, pack)
import Data.Aeson
import Data.Monoid ((<>))
import Control.Monad (mzero)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative (Applicative, (<$>), (<*>))

newtype Key = Key Text deriving (Show, Eq)

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

newtype Vindinium a = Vindinium { unVindinium :: ReaderT Settings IO a }
    deriving (Functor, Applicative, Monad, MonadReader Settings, MonadIO)

runVindinium :: Settings -> Vindinium a -> IO a
runVindinium s = flip runReaderT s . unVindinium

type Bot = State -> Vindinium Dir

data State = State {
    stateGame    :: Game
  , stateHero    :: Hero
  , stateToken   :: Text
  , stateViewUrl :: Text
  , statePlayUrl :: Text
} deriving (Show, Eq)

newtype GameId = GameId Text
    deriving (Show, Eq)

data Game = Game {
    gameId       :: GameId
  , gameTurn     :: Integer
  , gameMaxTurns :: Integer
  , gameHeroes   :: [Hero]
  , gameBoard    :: Board
  , gameFinished :: Bool
} deriving (Show, Eq)

newtype HeroId = HeroId Int
    deriving (Show, Eq)

data Hero = Hero {
    heroId        :: HeroId
  , heroName      :: Text
  , heroUserId    :: Maybe Text
  , heroElo       :: Maybe Integer
  , heroPos       :: Pos
  , heroLife      :: Integer
  , heroGold      :: Integer
  , heroMineCount :: Integer
  , heroSpawnPos  :: Pos
  , heroCrashed   :: Bool
} deriving (Show, Eq)

data Board = Board {
    boardSize  :: Int
  , boardTiles :: [Tile]
} deriving (Show, Eq)

data Tile = FreeTile
          | WoodTile
          | TavernTile
          | HeroTile HeroId
          | MineTile (Maybe HeroId)
    deriving (Show, Eq)

data Pos = Pos {
    posX :: Int
  , posY :: Int
} deriving (Show, Eq)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq)

instance ToJSON Key where
    toJSON (Key k) = String k

instance ToJSON Board where
    toJSON b  = object [ "size"  .= boardSize b
                       , "tiles" .= (printTiles $ boardTiles b)
                       ]

instance FromJSON State where
    parseJSON (Object o) = State <$> o .: "game"
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
parseBoard s t =
    Board s $ map parse (chunks t)
  where
    chunks []       = []
    chunks (_:[])   = error "chunks: even chars number"
    chunks (a:b:xs) = (a, b):chunks xs

    parse (' ', ' ') = FreeTile
    parse ('#', '#') = WoodTile
    parse ('@', x)   = HeroTile $ HeroId $ read [x]
    parse ('[', ']') = TavernTile
    parse ('$', '-') = MineTile Nothing
    parse ('$', x)   = MineTile $ Just $ HeroId $ read [x]
    parse (a, b)     = error $ "parse: unknown tile pattern " ++ (show $ a:b:[])

printTiles :: [Tile] -> Text
printTiles =
    foldl (<>) "" . map printTile
  where
    printTile FreeTile = "  "
    printTile WoodTile = "##"
    printTile (HeroTile (HeroId i)) = "@" <> (pack $ show i)
    printTile TavernTile = "[]"
    printTile (MineTile Nothing) = "$-"
    printTile (MineTile (Just (HeroId i))) = "$" <> (pack $ show i)
