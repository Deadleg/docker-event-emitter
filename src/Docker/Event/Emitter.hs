{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter (
    I.App(..),
    I.ListenerType(..),
    publishToRedis,
    publishToRest,
    I.unixSocketManager,
    createRedisConnection,
    I.mapEventType,
    listenToEvents
) where

import Data.Conduit hiding (connect)
import Data.ByteString.Lazy (fromStrict)
import Control.Monad (mapM_, forM_)
import Control.Monad.IO.Class (liftIO)
import Database.Redis hiding (info, get, String)
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)

import qualified Data.ByteString as S
import qualified Data.Conduit.List as CL
import qualified Docker.Event.Emitter.Internal as I

publishToRedis :: IO Connection -> Sink S.ByteString IO ()
publishToRedis connection = CL.mapM_ (\json -> do
    conn <- connection
    liftIO $ runRedis conn $ do
        publish "container:event" json
        return ())

publishToRest :: I.Endpoint -> Sink S.ByteString IO ()
publishToRest endpoint = CL.mapM_ (\json -> do
    request <- parseRequest ("POST " ++ endpoint)
    let request' = setRequestBodyLBS (fromStrict json) request
    httpLBS request -- should do something with the response?
    return ())

createRedisConnection :: I.Endpoint -> IO Connection
createRedisConnection endpoint = connect $ I.parseRedisConnection endpoint

listenToEvents :: Sink S.ByteString IO () -> IO ()
listenToEvents sink = do
    manager <- I.unixSocketManager
    request <- parseRequest "http://localhost/events"
    let request' = setRequestManager manager request
    withResponse request manager $ \response -> do
        let loop = do
                bodyReaderSource (responseBody response) =$ I.mapEventType $$ sink
                loop
        loop
