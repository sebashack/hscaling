{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Grpc.Server (
    runServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Internal (packChars)
import Grpc.Protobuf.Monitor (
    MonitorService (..),
    PushMetricsOkResponse (..),
    PushMetricsRequest (..),
    monitorServiceServer,
 )

import Network.GRPC.HighLevel.Generated (
    GRPCMethodType (Normal),
    Host (..),
    Port (..),
    ServerRequest (..),
    ServerResponse (..),
    ServiceOptions (..),
    StatusCode (..),
    defaultServiceOptions,
 )

handlers :: MonitorService ServerRequest ServerResponse
handlers =
    MonitorService
        { monitorServicePushMetrics = getMetricsHandler
        }

getMetricsHandler :: ServerRequest 'Normal PushMetricsRequest PushMetricsOkResponse -> IO (ServerResponse 'Normal PushMetricsOkResponse)
getMetricsHandler (ServerNormalRequest _metadata (PushMetricsRequest cpuLoad httpLoad dnsName)) = do
    liftIO $ print ("CPU LOAD = " <> show cpuLoad)
    liftIO $ print ("HTTP LOAD = " <> show httpLoad)
    liftIO $ print ("DNS NAME = " <> show dnsName)
    return $ ServerNormalResponse PushMetricsOkResponse [] StatusOk "Status ok"

runServer :: String -> Int -> IO ()
runServer host port =
    let options = defaultServiceOptions{serverHost = Host $ packChars host, serverPort = Port port}
     in monitorServiceServer handlers options
