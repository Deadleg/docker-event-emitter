{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter.Internal (
    App(..),
    ListenerType(..),
    EventStatus(..),
    EventType(..),
    Event(..),
    Endpoint(..),
    getResponseBody,
    mapEventType,
    unixSocketManager,
    addContainerToJSON,
    parseRedisConnection
) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Conduit hiding (connect)
import Data.Maybe
import Data.Monoid ((<>))
import Data.List.Split (splitOn)
import Data.Text (Text, unpack, toTitle)
import Prelude hiding (id)
import Text.Read hiding (choice, String)
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.Internal (Connection, openSocketConnection, makeConnection)
import Network.Socket.ByteString (sendAll, recv)

import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.List as CL
import qualified Database.Redis as R
import qualified Network.Socket.Internal as ST
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as RP
import qualified Control.Exception as E
import qualified Network.Socket as NS

type Endpoint = String

data ListenerType = Redis | Web deriving (Show, Eq)

instance Read ListenerType where
    readPrec = RP.lift $ P.choice
        [ P.string "redis" >> return Redis
        , P.string "web"   >> return Web
        ]

data App = App
    { listener :: ListenerType
    , endpoint :: Endpoint
    } deriving (Read, Show, Eq)

data EventStatus = Create | Pull | Start | Stop | Die | Destroy | Kill | Attach | UnknownStatus deriving (Eq, Show, Read)

instance FromJSON EventStatus where
    parseJSON (String s) = pure status
        where
            status = fromMaybe UnknownStatus ((readMaybe . unpack . toTitle) s)

data EventType = Container | Network | UnknownType deriving (Eq, Show, Read)

instance FromJSON EventType where
    parseJSON (String s) = pure type_
        where
            type_ = fromMaybe UnknownType ((readMaybe . unpack . toTitle) s)

data Event = Event
    { type_ :: EventType
    , id :: Text
    , status :: EventStatus
    } deriving (Eq, Show)

instance FromJSON Event where
    parseJSON (Object o) = Event <$> o .: "Type"
                                 <*> o .: "id"
                                 <*> o .: "status"

unixSocketManager :: IO Manager
unixSocketManager = newManager defaultManagerSettings { managerRawConnection = createUnixConnection }

createUnixConnection :: IO (Maybe NS.HostAddress -> String -> Int -> IO Connection)
createUnixConnection = return $ \_ _ _ -> openUnixConnection "/var/run/docker.sock"

openUnixConnection :: String -> IO Connection
openUnixConnection addr = E.bracketOnError
    (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol)
    (NS.close)
    $ \sock -> do
        NS.connect sock sockAddr
        socketConnection sock chunksize
  where
    sockAddr = NS.SockAddrUnix addr
    chunksize = 8192

socketConnection :: NS.Socket -> Int -> IO Connection
socketConnection socket chunksize = makeConnection
    (recv socket chunksize)
    (sendAll socket)
    (NS.close socket)

-- | Get the container json and add it to the event in the field "docker.event.emitter.container"
addContainerToJSON :: S.ByteString -> S.ByteString -> S.ByteString
addContainerToJSON event containerInfo = (C.init event) <> ", \"docker.event.emitter.container\":"
                                                        <> containerInfo
                                                        <> "}"

-- | Add container information to a container start event, otherwise don't alter the event.
mapEventType :: Conduit S.ByteString IO S.ByteString
mapEventType = CL.mapM (\event -> do
    let info = eitherDecode (B.fromStrict event) :: Either String Event
    case info of
        Right (Event Container id Start) -> do
            request <- parseRequest ("http://localhost/containers/" ++ T.unpack id ++ "/json")
            manager <- liftIO $ unixSocketManager
            let request' = setRequestManager manager request
            response <- httpLBS request'
            return $ addContainerToJSON (B.toStrict $ getResponseBody response) event
        Right _                          -> return event
        Left _                           -> return event) -- Event is not fully defined so this should happen often

parseRedisConnection :: Endpoint -> R.ConnectInfo
parseRedisConnection endpoint = R.defaultConnectInfo {R.connectHost = host, R.connectPort = port}
    where
        toPortNumber :: String -> ST.PortNumber
        toPortNumber s = (fromInteger . read) s

        split = splitOn ":" endpoint

        (host, port) = case split of
            [h, p] -> (h, R.PortNumber (toPortNumber p))
            [h]    -> (h, R.PortNumber 6379)
