{-# LANGUAGE OverloadedStrings #-}
module Faorien.Api ( startTraining
                   , startArena
                   , move
                   ) where

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Monoid ((<>))
import Network.HTTP.Client
import Network.HTTP.Types
import Control.Lens ((^.))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Faorien.Types

startTraining :: Maybe Int -> Maybe Board -> Faorien Activity
startTraining mi mb = do
    url <- startUrl "training"
    let obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )

    request url obj

move :: Activity -> Dir -> Faorien Activity
move s d = do
    let url = s^.activityPlayUrl
        obj = object [("dir", toJSON d)]

    request url obj


startArena :: Faorien Activity
startArena = do
    url <- startUrl "arena"
    let obj = object []

    request url obj

startUrl :: Text -> Faorien Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks _settingsUrl

request :: Text -> Value -> Faorien Activity
request url val = do
    key <- asks _settingsKey

    initReq <- liftIO $ parseUrl $ unpack url
    let req = initReq
                { method = "POST"
                , requestHeaders =
                    [ (hContentType, "application/json")
                    , (hAccept,      "application/json")
                    , (hUserAgent,   "vindinium-starter-haskell")
                    ]
                , requestBody = jsonBody (injectKey val key)
                , responseTimeout = Just 100000000000
                }

    liftIO $ withManager defaultManagerSettings $ \mgr ->
        liftM (decodeBody . responseBody) $ httpLbs req mgr

  where
    jsonBody = RequestBodyLBS . encode
    decodeBody body = case eitherDecode body of
            Left e  -> error $ "request: unable to decode state: " ++ e
            Right s -> s
    injectKey (Object a) k =
        let
            (Object b) = object [("key", toJSON k)]
        in
            Object (a <> b)
    injectKey _ _ = error "invalid object in injectKey method"
