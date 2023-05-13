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
    Instance (privateIp),
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
            insertMetric conn (privateIp ins) cpuLoad httpLoad
            putStrLn (">>>>>>> inserting metric for instance " <> (TL.unpack dnsName))
            return $ ServerNormalResponse PushMetricsOkResponse [] StatusOk "Status ok"
        Nothing -> do
            putStrLn (">>>>>>> WARNING: non-registered instance wth dnsName " <> (TL.unpack dnsName))
            return $ ServerNormalResponse PushMetricsOkResponse [] StatusNotFound "Instance not found"

runServer :: Connection -> String -> Int -> IO ()
runServer conn host port =
    let options = defaultServiceOptions{serverHost = Host $ packChars host, serverPort = Port port}
     in monitorServiceServer (handlers conn) options
