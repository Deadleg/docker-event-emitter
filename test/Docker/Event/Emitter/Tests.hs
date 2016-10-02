{-# LANGUAGE OverloadedStrings #-}

module Docker.Event.Emitter.Tests (
    tests
) where

import           Docker.Event.Emitter

import           Data.Aeson
import           Data.Conduit
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Word8                (Word8)

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer
import           Database.Redis            (PortID (PortNumber), connectHost,
                                            connectPort, defaultConnectInfo)
import           Numeric                   (showHex)
import           System.Process
import           Test.HUnit
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit          (testCase)
import           Test.Tasty.QuickCheck     (testProperty)

import qualified Data.ByteString           as S
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as LB
import qualified Data.Conduit.List         as CL
import qualified Data.HashMap.Strict       as M
import qualified Test.QuickCheck.Monadic   as QC

tests :: TestTree
tests = testGroup "Docker.Event.Emitter tests"
    [ testCase "Can listen to docker socket" getContainerEvent
    ]

testSink :: MVar [S.ByteString] -> Sink S.ByteString IO ()
testSink var = CL.mapM_ $ \json -> do
    liftIO $ modifyMVar_ var $ \results -> return (results <> [json])
    return ()

getContainerEvent :: Assertion
getContainerEvent = do
    var <- newMVar []
    testThread <- forkIO (listenToEvents (testSink var))
    callCommand "docker run -d --name=testngx nginx"
    callCommand "docker rm -f testngx"
    killThread testThread
    result <- takeMVar var
    print result
