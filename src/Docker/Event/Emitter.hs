{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter (
    App(..),
    ListenerType(..),
    newEvent,
    dispatchToRedis,
    dispatchToRest,
    getEvents
) where

import Docker.Event.Emitter.Internal

import Conduit hiding (connect)
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (splitOn)
import Database.Redis hiding (info, get, String)
import Network.HTTP.Simple hiding (getResponseBody)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket.Internal as ST

getEvents :: Source IO S.ByteString
getEvents = yield "GET /events HTTP/1.1\r\n\r\n"

dispatchToRedis :: Endpoint -> S.ByteString -> Sink S.ByteString IO ()
dispatchToRedis endpoint json = do
    conn <- liftIO $ connect defaultConnectInfo { connectHost = host, connectPort = port}
    liftIO $ runRedis conn $ do
        publish "container:event" json
        return ()
    where
        toPortNumber :: String -> ST.PortNumber
        toPortNumber s = (fromInteger . read) s

        split = splitOn ":" endpoint
        (host, port) = case split of
            [h, p] -> (h, PortNumber (toPortNumber p))
            [h]    -> (h, PortNumber 6379)

dispatchToRest :: Endpoint -> S.ByteString -> Sink S.ByteString IO ()
dispatchToRest endpoint json = do
    request <- parseRequest ("POST " ++ endpoint)
    httpLBS request -- should do something with the response?
    return ()

newEvent :: (S.ByteString -> Sink S.ByteString IO ()) -> S.ByteString -> Sink S.ByteString IO ()
newEvent dispatch event
    | "HTTP" `C.isPrefixOf` event = return ()
    | otherwise                   = splitEvent dispatch event
