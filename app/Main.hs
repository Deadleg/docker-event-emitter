{-# LANGUAGE OverloadedStrings #-}
module Main where

import Docker.Event.Emitter

import Conduit hiding (connect)
import Data.Conduit hiding (connect)
import Data.Conduit.Network.Unix
import Options.Applicative hiding (choice)

appParser :: Parser App
appParser = App <$> (option auto
                       ( long "backend"
                      <> short 'b'
                      <> metavar "BACKEND"
                      <> help "Backend type: redis | web"
                      <> completeWith ["redis", "web"]))
                <*> (strOption
                       ( long "endpoint"
                      <> short 'e'
                      <> metavar "ENDPOINT"
                      <> help "Redis: hostname:port | web: full url"))

-- | Parses the options to extract the correct 'dispatcher', and then connects to the docker daemon.
-- The 'dispatcher' is just a name for the type of publisher we are using (e.g. Redis/Web).
-- Could probably change the name to publisher.
main :: IO ()
main = do
    app <- execParser opts
    let dispatch = dispatcher app
    runUnixClient (clientSettings "/var/run/docker.sock") $ \server -> do
        getEvents $$ appSink server
        appSource server $$ awaitForever (newEvent dispatch)
    return ()
    where
        opts = info (helper <*> appParser)
           ( fullDesc
          <> header "Docker Event Emitter - relay docker events to somewhere else"
          <> progDesc "Emit docker events to a subscriber such as Redis or a RESTful endpoint.")

        dispatcher app = case listener app of
            Redis -> dispatchToRedis $ endpoint app
            Web   -> dispatchToRest $ endpoint app
