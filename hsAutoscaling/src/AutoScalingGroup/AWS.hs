{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module AutoScalingGroup.AWS (
    runInstance,
    terminateInstance,
    InstanceInfo (..),
) where

import Control.Lens.Getter (view)
import Control.Lens.Operators ((.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks, void)
import Control.Monad.Trans.AWS (
    AWST,
    runAWST,
    runResourceT,
    send,
 )
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Text.Lazy as LT (toStrict)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.AWS.EC2.RunInstances (
    rInstances,
    risImageId,
    risInstanceType,
    risKeyName,
    risSecurityGroupIds,
    risSubnetId,
    risTagSpecifications,
    risUserData,
    runInstances,
 )
import Network.AWS.EC2.TerminateInstances (terminateInstances, tiInstanceIds)
import Network.AWS.EC2.Types (
    ResourceType (Instance),
    insInstanceId,
    insPrivateDNSName,
    insPrivateIPAddress,
    tag,
    tagSpecification,
    tsResourceType,
    tsTags,
 )
import qualified Network.AWS.Env as AWS

import AutoScalingGroup.Env (
    EC2Opts (..),
    Env (..),
 )

data InstanceInfo = InstanceInfo
    { privateIp :: Text
    , privateDNSName :: Text
    , instanceId :: Text
    }
    deriving (Show)

runInstance :: (MonadReader Env m, MonadIO m) => m InstanceInfo
runInstance = do
    conf <- asks ec2Conf
    uuid <- liftIO $ fmap toText nextRandom
    monConf <- asks monitorConf
    let instanceName = namePrefix conf <> "-" <> uuid
        monConfTxt = (LT.toStrict . encodeToLazyText) monConf
        req =
            runInstances 1 1
                & risInstanceType .~ Just (instanceType conf)
                & risKeyName .~ Just (keypair conf)
                & risImageId .~ Just (amiId conf)
                & risSubnetId .~ Just (subnetId conf)
                & risSecurityGroupIds .~ (securityGroups conf)
                & risTagSpecifications .~ [tags instanceName]
                & risUserData .~ Just (encodeBase64 $ launchScript monConfTxt)
    env <- asks awsEnv
    res <- runAWSAction env (send req)
    let runningInstance = Prelude.head $ view rInstances res
        insId = view insInstanceId runningInstance
        ipAddr = view insPrivateIPAddress runningInstance
        dnsName = view insPrivateDNSName runningInstance
    return InstanceInfo{privateIp = fromJust ipAddr, instanceId = insId, privateDNSName = fromJust dnsName}
  where
    launchScript conf = "#!/bin/bash\nrm -f /opt/monitor_config.json\necho '" <> conf <> "' > /opt/monitor_config.json"
    --
    tags instanceName =
        tagSpecification
            & tsResourceType .~ Just Instance
            & tsTags .~ [tag "Name" instanceName]

terminateInstance :: (MonadReader Env m, MonadIO m) => Text -> m ()
terminateInstance insId = do
    let req = terminateInstances & tiInstanceIds .~ [insId]
    env <- asks awsEnv
    void $ runAWSAction env (send req)

--
-- Helpers
--
runAWSAction :: MonadIO m => AWS.Env -> AWST (ResourceT IO) a -> m a
runAWSAction env action = liftIO $ runResourceT . runAWST env $ action
