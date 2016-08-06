{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter.Internal.Tests (
    tests
) where

import Docker.Event.Emitter.Internal

import Data.Conduit
import Data.Monoid ((<>))
import Data.Word8 (Word8)
import Numeric (showHex)
import Test.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.List as CL
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

tests :: TestTree
tests = testGroup "Docker.Event.Emitter.Internal tests"
    [ testCase "'web' is read as Web" backendReadForWeb
    , testCase "'redis' is read as Redis" backendReadForRedis
    , testProperty "chunked http response" checkChunkedResponses
    ]

backendReadForWeb :: Assertion
backendReadForWeb = read "web" @?= Web

backendReadForRedis :: Assertion
backendReadForRedis = read "redis" @?= Redis

makeDummyResponse :: String -> String
makeDummyResponse body = "someheader:value\r\n\r\n" ++ (showHex (length (body)) "\r\n") ++ body ++ "\r\n0\r\n"

instance QC.Arbitrary S.ByteString where
    arbitrary = do
        body <- QC.listOf (QC.elements ['a'..'z'])
        return $ C.pack (makeDummyResponse ("{" ++ body ++ "}"))

checkChunkedResponses :: S.ByteString -> QC.Property
checkChunkedResponses response = QC.monadicIO $ do
    [result] <- QC.run ((yield response) $= getResponseBody $$ CL.consume)
    QC.assert $ C.pack (makeDummyResponse $ C.unpack result) == response


