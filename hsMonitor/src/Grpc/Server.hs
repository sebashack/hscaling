{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Grpc.Server (
    runServer,
) where

import Data.ByteString.Internal (packChars)
import Grpc.Protobuf.Monitor (
    HeartbeatOkResponse (..),
    HeartbeatRequest (..),
    MonitorService (..),
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
        { monitorServiceGetHeartbeat = getHeartbeatHandler
        }

getHeartbeatHandler :: ServerRequest 'Normal HeartbeatRequest HeartbeatOkResponse -> IO (ServerResponse 'Normal HeartbeatOkResponse)
getHeartbeatHandler (ServerNormalRequest _metadata HeartbeatRequest) = return $ ServerNormalResponse (HeartbeatOkResponse "Ok") [] StatusOk "Status ok"

runServer :: String -> Int -> IO ()
runServer host port =
    let options = defaultServiceOptions{serverHost = Host $ packChars host, serverPort = Port port}
     in monitorServiceServer handlers options
