{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Grpc.Server (
    runServer,
) where

import Data.ByteString.Internal (packChars)
import Data.Text.Lazy as TL (toStrict, unpack)
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

import AutoScalingGroup.CRUD (
    Instance (instanceId),
    insertMetric,
    selectInstanceByDNSName,
 )
import Database.SQLite.Simple (Connection)

handlers :: Connection -> MonitorService ServerRequest ServerResponse
handlers conn =
    MonitorService
        { monitorServicePushMetrics = getMetricsHandler conn
        }

getMetricsHandler :: Connection -> ServerRequest 'Normal PushMetricsRequest PushMetricsOkResponse -> IO (ServerResponse 'Normal PushMetricsOkResponse)
getMetricsHandler conn (ServerNormalRequest _metadata (PushMetricsRequest cpuLoad httpLoad dnsName)) = do
    maybeIns <- selectInstanceByDNSName conn (TL.toStrict dnsName)
    case maybeIns of
        Just ins -> do
            insertMetric conn (instanceId ins) cpuLoad httpLoad
            putStrLn
                ( ">>>>>>> inserting metrics for instance "
                    <> (TL.unpack dnsName)
                    <> ": cpu-load "
                    <> show cpuLoad
                    <> ", http-load "
                    <> show httpLoad
                )
            return $ ServerNormalResponse PushMetricsOkResponse [] StatusOk "Status ok"
        Nothing -> do
            putStrLn (">>>>>>> WARNING: non-registered instance wth dnsName " <> (TL.unpack dnsName))
            return $ ServerNormalResponse PushMetricsOkResponse [] StatusNotFound "Instance not found"

runServer :: Connection -> String -> Int -> IO ()
runServer conn host port =
    let options = defaultServiceOptions{serverHost = Host $ packChars host, serverPort = Port port}
     in do
            putStrLn ("Running grpc server on " <> host <> ":" <> show port)
            monitorServiceServer (handlers conn) options
