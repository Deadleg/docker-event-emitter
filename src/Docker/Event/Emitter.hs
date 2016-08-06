{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter (
    I.App(..),
    I.ListenerType(..),
    newEvent,
    publishToRedis,
    publishToRest,
    I.unixSocketManager
) where


import Data.Conduit hiding (connect)
import Data.ByteString.Lazy (fromStrict)
import Control.Monad (mapM_, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (splitOn)
import Database.Redis hiding (info, get, String)
import Network.Socket.Internal (PortNumber)
import Network.HTTP.Simple

import qualified Data.ByteString as S
import qualified Data.Conduit.List as CL
import qualified Docker.Event.Emitter.Internal as I

publishToRedis :: I.Endpoint -> Sink S.ByteString IO ()
publishToRedis endpoint = CL.mapM_ (\json -> do
        conn <- liftIO $ connect defaultConnectInfo { connectHost = host, connectPort = port}
        liftIO $ runRedis conn $ do
            publish "container:event" json
            return ())
    where
        toPortNumber :: String -> PortNumber
        toPortNumber s = (fromInteger . read) s

        split = splitOn ":" endpoint
        (host, port) = case split of
            [h, p] -> (h, PortNumber (toPortNumber p))
            [h]    -> (h, PortNumber 6379)

publishToRest :: I.Endpoint -> Sink S.ByteString IO ()
publishToRest endpoint = CL.mapM_ (\json -> do
        request <- parseRequest ("POST " ++ endpoint)
        let request' = setRequestBodyLBS (fromStrict json) request
        httpLBS request -- should do something with the response?
        return ())

newEvent :: Sink S.ByteString IO () -> Sink S.ByteString IO ()
newEvent publisher = I.mapEventType $= publisher
