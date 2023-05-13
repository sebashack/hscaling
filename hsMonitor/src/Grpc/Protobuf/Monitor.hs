{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing       #-}
{-# OPTIONS_GHC -fno-warn-unused-matches       #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module Grpc.Protobuf.Monitor where
import qualified Prelude as Hs
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import qualified Control.Applicative as Hs
import Control.Applicative ((<*>), (<|>), (<$>))
import qualified Control.DeepSeq as Hs
import qualified Control.Monad as Hs
import qualified Data.ByteString as Hs
import qualified Data.Coerce as Hs
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as Hs (NonEmpty(..))
import qualified Data.Map as Hs (Map, mapKeysMonotonic)
import qualified Data.Proxy as Proxy
import qualified Data.String as Hs (fromString)
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Enum as Hs
import qualified GHC.Generics as Hs
import qualified Unsafe.Coerce as Hs
import Network.GRPC.HighLevel.Generated as HsGRPC
import Network.GRPC.HighLevel.Client as HsGRPC
import Network.GRPC.HighLevel.Server as HsGRPC hiding (serverLoop)
import Network.GRPC.HighLevel.Server.Unregistered as HsGRPC
       (serverLoop)
 
data MonitorService request
     response = MonitorService{monitorServicePushMetrics ::
                               request 'HsGRPC.Normal Grpc.Protobuf.Monitor.PushMetricsRequest
                                 Grpc.Protobuf.Monitor.PushMetricsOkResponse
                                 ->
                                 Hs.IO
                                   (response 'HsGRPC.Normal
                                      Grpc.Protobuf.Monitor.PushMetricsOkResponse)}
              deriving Hs.Generic
 
monitorServiceServer ::
                       MonitorService HsGRPC.ServerRequest HsGRPC.ServerResponse ->
                         HsGRPC.ServiceOptions -> Hs.IO ()
monitorServiceServer
  MonitorService{monitorServicePushMetrics =
                   monitorServicePushMetrics}
  (ServiceOptions serverHost serverPort useCompression
     userAgentPrefix userAgentSuffix initialMetadata sslConfig logger
     serverMaxReceiveMessageLength serverMaxMetadataSize)
  = (HsGRPC.serverLoop
       HsGRPC.defaultOptions{HsGRPC.optNormalHandlers =
                               [(HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/monitor.MonitorService/PushMetrics")
                                   (HsGRPC.convertGeneratedServerHandler
                                      monitorServicePushMetrics))],
                             HsGRPC.optClientStreamHandlers = [],
                             HsGRPC.optServerStreamHandlers = [],
                             HsGRPC.optBiDiStreamHandlers = [], optServerHost = serverHost,
                             optServerPort = serverPort, optUseCompression = useCompression,
                             optUserAgentPrefix = userAgentPrefix,
                             optUserAgentSuffix = userAgentSuffix,
                             optInitialMetadata = initialMetadata, optSSLConfig = sslConfig,
                             optLogger = logger,
                             optMaxReceiveMessageLength = serverMaxReceiveMessageLength,
                             optMaxMetadataSize = serverMaxMetadataSize})
 
monitorServiceClient ::
                       HsGRPC.Client ->
                         Hs.IO (MonitorService HsGRPC.ClientRequest HsGRPC.ClientResult)
monitorServiceClient client
  = (Hs.pure MonitorService) <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/monitor.MonitorService/PushMetrics")))
 
data PushMetricsRequest = PushMetricsRequest{pushMetricsRequestCpuLoad
                                             :: Hs.Float,
                                             pushMetricsRequestHttpLoad :: Hs.Float,
                                             pushMetricsRequestPrivateDnsName :: Hs.Text}
                        deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)
 
instance HsProtobuf.Named PushMetricsRequest where
        nameOf _ = (Hs.fromString "PushMetricsRequest")
 
instance HsProtobuf.HasDefault PushMetricsRequest
 
instance HsProtobuf.Message PushMetricsRequest where
        encodeMessage _
          PushMetricsRequest{pushMetricsRequestCpuLoad =
                               pushMetricsRequestCpuLoad,
                             pushMetricsRequestHttpLoad = pushMetricsRequestHttpLoad,
                             pushMetricsRequestPrivateDnsName =
                               pushMetricsRequestPrivateDnsName}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   pushMetricsRequestCpuLoad),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   pushMetricsRequestHttpLoad),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 3)
                   pushMetricsRequestPrivateDnsName)])
        decodeMessage _
          = (Hs.pure PushMetricsRequest) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 3))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Float)
                (HsProtobuf.Single "cpu_load")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Float)
                (HsProtobuf.Single "http_load")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 3)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "private_dns_name")
                []
                "")]
 
instance HsJSONPB.ToJSONPB PushMetricsRequest where
        toJSONPB (PushMetricsRequest f1 f2 f3)
          = (HsJSONPB.object
               ["cpu_load" .= f1, "http_load" .= f2, "private_dns_name" .= f3])
        toEncodingPB (PushMetricsRequest f1 f2 f3)
          = (HsJSONPB.pairs
               ["cpu_load" .= f1, "http_load" .= f2, "private_dns_name" .= f3])
 
instance HsJSONPB.FromJSONPB PushMetricsRequest where
        parseJSONPB
          = (HsJSONPB.withObject "PushMetricsRequest"
               (\ obj ->
                  (Hs.pure PushMetricsRequest) <*> obj .: "cpu_load" <*>
                    obj .: "http_load"
                    <*> obj .: "private_dns_name"))
 
instance HsJSONPB.ToJSON PushMetricsRequest where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding
 
instance HsJSONPB.FromJSON PushMetricsRequest where
        parseJSON = HsJSONPB.parseJSONPB
 
instance HsJSONPB.ToSchema PushMetricsRequest where
        declareNamedSchema _
          = do let declare_cpu_load = HsJSONPB.declareSchemaRef
               pushMetricsRequestCpuLoad <- declare_cpu_load Proxy.Proxy
               let declare_http_load = HsJSONPB.declareSchemaRef
               pushMetricsRequestHttpLoad <- declare_http_load Proxy.Proxy
               let declare_private_dns_name = HsJSONPB.declareSchemaRef
               pushMetricsRequestPrivateDnsName <- declare_private_dns_name
                                                     Proxy.Proxy
               let _ = Hs.pure PushMetricsRequest <*>
                         HsJSONPB.asProxy declare_cpu_load
                         <*> HsJSONPB.asProxy declare_http_load
                         <*> HsJSONPB.asProxy declare_private_dns_name
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "PushMetricsRequest",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("cpu_load", pushMetricsRequestCpuLoad),
                                                        ("http_load", pushMetricsRequestHttpLoad),
                                                        ("private_dns_name",
                                                         pushMetricsRequestPrivateDnsName)]}})
 
data PushMetricsOkResponse = PushMetricsOkResponse{}
                           deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)
 
instance HsProtobuf.Named PushMetricsOkResponse where
        nameOf _ = (Hs.fromString "PushMetricsOkResponse")
 
instance HsProtobuf.HasDefault PushMetricsOkResponse
 
instance HsProtobuf.Message PushMetricsOkResponse where
        encodeMessage _ PushMetricsOkResponse{} = (Hs.mconcat [])
        decodeMessage _ = (Hs.pure PushMetricsOkResponse)
        dotProto _ = []
 
instance HsJSONPB.ToJSONPB PushMetricsOkResponse where
        toJSONPB (PushMetricsOkResponse) = (HsJSONPB.object [])
        toEncodingPB (PushMetricsOkResponse) = (HsJSONPB.pairs [])
 
instance HsJSONPB.FromJSONPB PushMetricsOkResponse where
        parseJSONPB
          = (HsJSONPB.withObject "PushMetricsOkResponse"
               (\ obj -> (Hs.pure PushMetricsOkResponse)))
 
instance HsJSONPB.ToJSON PushMetricsOkResponse where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding
 
instance HsJSONPB.FromJSON PushMetricsOkResponse where
        parseJSON = HsJSONPB.parseJSONPB
 
instance HsJSONPB.ToSchema PushMetricsOkResponse where
        declareNamedSchema _
          = do Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "PushMetricsOkResponse",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList []}})
