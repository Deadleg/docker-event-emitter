{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter.Internal (
    App(..),
    ListenerType(..),
    EventStatus(..),
    EventType(..),
    Event(..),
    Endpoint(..),
    inspectContainer,
    dispatchContainerStart,
    getResponseBody,
    splitEvent
) where

import Conduit hiding (connect)
import Control.Applicative ((<*>), (<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Char (ord)
import Data.Conduit hiding (connect)
import Data.Conduit.Network.Unix
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text, unpack, toTitle)
import Data.Text.Encoding (encodeUtf8)
import Data.Word8 (_cr, _lf)
import Numeric
import Prelude hiding (id)
import Text.Read hiding (choice, String)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.List as CL
import qualified Network.Socket.Internal as ST
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as RP

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

inspectContainer :: Text -> Source IO S.ByteString
inspectContainer id = yield $ "GET /containers/" <> (encodeUtf8) id <> "/json HTTP/1.1\r\n\r\n"

-- | Get the container json and add it to the event in the field "docker.event.emitter.container"
dispatchContainerStart :: (S.ByteString -> Sink S.ByteString IO ()) -> S.ByteString -> Sink S.ByteString IO ()
dispatchContainerStart dispatch event = do
    response <- await
    let inspectBody = fromMaybe "" response
    let json        = (C.init event) <> ", \"docker.event.emitter.container\":" <> inspectBody <> "}"
    dispatch json

-- | Construct the HTTP body from a chunked encoding response.
getResponseBody :: Conduit S.ByteString IO S.ByteString
getResponseBody = filterHeaders >> buildBody ""
    where
        filterHeaders = do
            line <- lineAsciiC $ takeCE 128 =$= filterCE (/= _cr) =$= foldC
            if line == ""
                then return ()
                else filterHeaders

        readHTTPLine :: Int -> ConduitM S.ByteString S.ByteString IO S.ByteString
        readHTTPLine take = lineAsciiC $ takeCE take =$= filterCE (/= _cr) =$= foldC

        buildBody :: S.ByteString -> Conduit S.ByteString IO S.ByteString
        buildBody partial = do
            chunkLength <- readHTTPLine 32
            let [(size, _)] = readHex (C.unpack chunkLength)
            body <- takeCE size =$= foldC
            readHTTPLine 32 -- blank line after chunk
            nextChunkLength <- readHTTPLine 128
            if nextChunkLength == "0"
                then yield $ partial <> body
                else leftover nextChunkLength >> buildBody body

-- | Split the received response into its (possibly) multiple events.
splitEvent :: (S.ByteString -> Sink S.ByteString IO ()) -> S.ByteString -> Sink S.ByteString IO ()
splitEvent dispatch "" = return ()
splitEvent dispatch event = do
    let info = eitherDecode (B.fromStrict json) :: Either String Event
    case info of
        Right (Event Container id Start) -> do
            liftIO $ runUnixClient (clientSettings "/var/run/docker.sock") $ \server -> do
                inspectContainer id $$ appSink server
                appSource server $= getResponseBody $$ dispatchContainerStart dispatch json
        Right _                          -> dispatch json
        Left err                         -> dispatch json -- Event is not fully defined so this should happen often
    splitEvent dispatch remainder
    where
        (z, y)      = C.breakSubstring "\r\n" event
        [(size, _)] = readHex (C.unpack z)
        json        = C.init $ C.take size (C.drop 2 y) -- drop leading \r\n  end trailing \n
        remainder   = C.drop (2 + size + 2) y -- drop surrounding \r\n
