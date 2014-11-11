{-# LANGUAGE OverloadedStrings #-}
module Fao.Api ( startTraining
               , startArena
               , move
               ) where

import Data.Text (Text, unpack)
import Data.Monoid ((<>))
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Fao.Types

startTraining :: Maybe Int -> Maybe Board -> Vindinium State
startTraining mi mb = do
    url <- startUrl "training"
    let obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )

    request url obj

move :: State -> Dir -> Vindinium State
move s d = do
    let url = statePlayUrl s
        obj = object [("dir", toJSON d)]

    request url obj


startArena :: Vindinium State
startArena = do
    url <- startUrl "arena"
    let obj = object []

    request url obj

startUrl :: Text -> Vindinium Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks settingsUrl

request :: Text -> Value -> Vindinium State
request url val = do
    key <- asks settingsKey

    initReq <- liftIO $ parseUrl $ unpack url
    let req = initReq
                { method = "POST"
                , requestHeaders =
                    [ (hContentType, "application/json")
                    , (hAccept,      "application/json")
                    , (hUserAgent,   "vindinium-starter-haskell")
                    ]
                , requestBody = jsonBody (injectKey val key)
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
    injectKey _ _ = error "invalid argument to injectKey" -- avoid warning
