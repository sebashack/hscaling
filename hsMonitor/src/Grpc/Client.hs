{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Grpc.Client (
    MonitorResponse (..),
    getHeartbeat,
    runActions,
) where

import Data.ByteString.Internal (packChars)
import Grpc.Protobuf.Monitor (
    HeartbeatOkResponse (..),
    HeartbeatRequest (..),
    MonitorService (..),
    monitorServiceClient,
 )
import Network.GRPC.HighLevel.Client (Client)
import Network.GRPC.HighLevel.Generated (
    ClientConfig (..),
    ClientRequest (..),
    ClientResult (..),
    Host (..),
    Port (..),
    withGRPCClient,
 )

data MonitorResponse a = OkResponse a | ErrResponse String deriving (Show)

getHeartbeat :: Int -> Client -> IO (MonitorResponse ())
getHeartbeat reqTimeoutSecs client = do
    let req = ClientNormalRequest HeartbeatRequest reqTimeoutSecs []
    endpoint <- monitorServiceGetHeartbeat <$> monitorServiceClient client
    result <- endpoint req
    return $ case result of
        ClientNormalResponse (HeartbeatOkResponse _) _meta1 _meta2 _status _details -> OkResponse ()
        ClientErrorResponse err -> ErrResponse $ show err

runActions :: String -> Int -> (Client -> IO a) -> IO a
runActions host port actions = do
    let config =
            ClientConfig
                { clientServerHost = Host $ packChars host
                , clientServerPort = Port port
                , clientArgs = []
                , clientSSLConfig = Nothing
                , clientAuthority = Nothing
                }
    withGRPCClient config actions
