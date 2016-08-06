{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter.Internal.Tests (
    tests
) where

import Docker.Event.Emitter.Internal

import Data.Aeson
import Data.Maybe
import Data.Conduit
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.Word8 (Word8)
import Numeric (showHex)
import Test.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.HashMap.Strict as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.List as CL
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC

tests :: TestTree
tests = testGroup "Docker.Event.Emitter.Internal tests"
    [ testCase "'web' is read as Web" backendReadForWeb
    , testCase "'redis' is read as Redis" backendReadForRedis
    , testProperty "check adding container info is valid json" checkAddContainerToJSONIsJSON
    ]

backendReadForWeb :: Assertion
backendReadForWeb = read "web" @?= Web

backendReadForRedis :: Assertion
backendReadForRedis = read "redis" @?= Redis

checkAddContainerToJSONIsJSON :: S.ByteString -> Property
checkAddContainerToJSONIsJSON info = M.lookup "docker.event.emitter.container" ast
                                     ===
                                     Just (Object (M.fromList [("data", String $ decodeUtf8 info)]))
    where Just (Object ast) = decode . LB.fromStrict $ addContainerToJSON "{\"a\": 1}" ("{ \"data\": \"" <> info <> "\"}") :: Maybe Value

instance Arbitrary S.ByteString where
    arbitrary = do
        body <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
        return $ C.pack body

