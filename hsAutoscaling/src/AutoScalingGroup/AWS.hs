{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module AutoScalingGroup.AWS (runInstance) where

import AutoScalingGroup.Env (EC2Opts (..), Env (..), InstanceInfo (..))
import Control.Lens.Getter (view)
import Control.Lens.Operators ((.~))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.AWS (
    AWST,
    runAWST,
    runResourceT,
    send,
 )
import Control.Monad.Trans.Resource (ResourceT)
import Data.Function ((&))
import Data.Text
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
    runInstances,
 )
import Network.AWS.EC2.Types (
    ResourceType (Instance),
    insInstanceId,
    insPrivateIPAddress,
    tag,
    tagSpecification,
    tsResourceType,
    tsTags,
 )
import qualified Network.AWS.Env as AWS

runInstance :: (MonadReader Env m, MonadIO m) => ExceptT Text m InstanceInfo
runInstance = do
    conf <- asks ec2Conf
    uuid <- liftIO $ fmap toText nextRandom
    let instanceName = namePrefix conf <> "-" <> uuid
    let req =
            runInstances 1 1
                & risInstanceType .~ Just (instanceType conf)
                & risKeyName .~ Just (keypair conf)
                & risImageId .~ Just (amiId conf)
                & risSubnetId .~ Just (subnetId conf)
                & risSecurityGroupIds .~ (securityGroups conf)
                & risTagSpecifications .~ [tags instanceName]
    env <- asks awsEnv
    res <- runAWSAction env (send req)
    let runningInstance = Prelude.head $ view rInstances res
    let insId = view insInstanceId runningInstance
    let ipAddr = view insPrivateIPAddress runningInstance
    return InstanceInfo{privateIp = ipAddr, instanceId = insId}
  where
    tags instanceName =
        tagSpecification
            & tsResourceType .~ Just Instance
            & tsTags .~ [tag "Name" instanceName]

--
-- Helpers
--
runAWSAction :: MonadIO m => AWS.Env -> AWST (ResourceT IO) a -> m a
runAWSAction env action = liftIO $ runResourceT . runAWST env $ action
