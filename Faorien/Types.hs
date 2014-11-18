{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Faorien.Types where

import Data.Text (Text, pack)
import Data.Aeson
import Data.Monoid ((<>))
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative (Applicative, (<$>), (<*>))
import qualified Data.Map as M

newtype Key = Key Text deriving (Show, Eq)

data Settings = Settings {
    _settingsKey :: Key
  , _settingsUrl :: Text
} deriving (Show, Eq)

newtype Faorien a = Faorien { unFaorien :: StateT BotState (ReaderT Settings IO) a }
    deriving (Functor, Applicative, Monad, MonadReader Settings, MonadState BotState, MonadIO)

runFaorien :: Settings -> BotState -> Faorien a -> IO a
runFaorien settings state = flip runReaderT settings . flip evalStateT state . unFaorien

data Bot = Bot { initialize :: Faorien ()
               , turn :: Faorien Dir
               }

data BotState = BotState { _session :: Activity
                         , _internal :: Internal
                         }

data Internal = Internal {
                         }

newtype Path  = Path [Pos] deriving (Show, Eq)

-- Distance function between two positions on a board
type Distance = Pos -> Pos -> Int

type HeroBoardMap = Pos -> Maybe Path

type BoardMap = M.Map HeroId HeroBoardMap

data Activity = Activity {
    _activityGame    :: Game
  , _activityHero    :: Hero
  , _activityToken   :: Text
  , _activityViewUrl :: Text
  , _activityPlayUrl :: Text
} deriving (Show, Eq)

newtype GameId = GameId Text
    deriving (Show, Eq)

data Game = Game {
    _gameId       :: GameId
  , _gameTurn     :: Integer
  , _gameMaxTurns :: Integer
  , _gameHeroes   :: [Hero]
  , _gameBoard    :: Board
  , _gameFinished :: Bool
} deriving (Show, Eq)

newtype HeroId = HeroId Int
    deriving (Show, Eq, Ord)

data Hero = Hero {
    _heroId        :: HeroId
  , _heroName      :: Text
  , _heroUserId    :: Maybe Text
  , _heroElo       :: Maybe Integer
  , _heroPos       :: Pos
  , _heroLife      :: Integer
  , _heroGold      :: Integer
  , _heroMineCount :: Integer
  , _heroSpawnPos  :: Pos
  , _heroCrashed   :: Bool
} deriving (Show, Eq)

data Board = Board {
    _boardSize  :: Int
  , _boardTiles :: [Tile]
} deriving (Show, Eq)

data Tile = FreeTile
          | WoodTile
          | TavernTile
          | HeroTile HeroId
          | MineTile (Maybe HeroId)
    deriving (Show, Eq)

data Pos = Pos {
    _posX :: Int
  , _posY :: Int
} deriving (Show, Eq, Ord)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq)

instance ToJSON Key where
    toJSON (Key k) = String k

instance ToJSON Board where
    toJSON b  = object [ "size"  .= _boardSize b
                       , "tiles" .= printTiles (_boardTiles b)
                       ]

instance FromJSON Activity where
    parseJSON (Object o) = Activity <$> o .: "game"
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

makeLenses ''Settings
makeLenses ''Activity
makeLenses ''Game
makeLenses ''Hero
makeLenses ''Board
makeLenses ''Pos
makeLenses ''BotState
makeLenses ''Internal
