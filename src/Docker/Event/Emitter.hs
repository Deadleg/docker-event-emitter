{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter (
    I.App(..),
    I.ListenerType(..),
    newEvent,
    publishToRedis,
    publishToRest,
    getEvents,
    I.unixSocketManager
) where


import Data.Conduit hiding (connect)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (splitOn)
import Database.Redis hiding (info, get, String)
import Network.Socket.Internal (PortNumber)
import Network.HTTP.Simple

import qualified Docker.Event.Emitter.Internal as I
import qualified Data.ByteString as S

getEvents :: Source IO S.ByteString
getEvents = yield "GET /events HTTP/1.1\r\n\r\n"

publishToRedis :: I.Endpoint -> S.ByteString -> Sink S.ByteString IO ()
publishToRedis endpoint json = do
    conn <- liftIO $ connect defaultConnectInfo { connectHost = host, connectPort = port}
    liftIO $ runRedis conn $ do
        publish "container:event" json
        return ()
    where
        toPortNumber :: String -> PortNumber
        toPortNumber s = (fromInteger . read) s

        split = splitOn ":" endpoint
        (host, port) = case split of
            [h, p] -> (h, PortNumber (toPortNumber p))
            [h]    -> (h, PortNumber 6379)

publishToRest :: I.Endpoint -> S.ByteString -> Sink S.ByteString IO ()
publishToRest endpoint json = do
    request <- parseRequest ("POST " ++ endpoint)
    httpLBS request -- should do something with the response?
    return ()

newEvent :: (S.ByteString -> Sink S.ByteString IO ()) -> Sink S.ByteString IO ()
newEvent publisher = await >>= mapM_ (I.publishEvent publisher)
