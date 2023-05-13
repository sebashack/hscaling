{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Grpc.Client (
    MonitorResponse (..),
    pushMetrics,
    runActions,
) where

import Data.ByteString.Internal (packChars)
import Data.Text as TS (Text)
import Data.Text.Lazy as TL
import Grpc.Protobuf.Monitor (
    MonitorService (..),
    PushMetricsOkResponse (..),
    PushMetricsRequest (..),
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

pushMetrics :: Float -> Float -> TS.Text -> Int -> Client -> IO (MonitorResponse ())
pushMetrics cpuLoad httpLoad privateDNSName reqTimeoutSecs client = do
    let pushReq = PushMetricsRequest cpuLoad httpLoad (TL.fromStrict privateDNSName)
        clientReq = ClientNormalRequest pushReq reqTimeoutSecs []
    endpoint <- monitorServicePushMetrics <$> monitorServiceClient client
    result <- endpoint clientReq
    return $ case result of
        ClientNormalResponse PushMetricsOkResponse _meta1 _meta2 _status _details -> OkResponse ()
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
