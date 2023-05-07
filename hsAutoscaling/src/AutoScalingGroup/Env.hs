{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AutoScalingGroup.Env (
    ASGAction,
    ASGActionE,
    Env (..),
    Opts (..),
    EC2Opts (..),
    actionE,
    logErrText,
    logText,
    mkEnv,
    runASGAction,
    InstanceInfo (..),
) where

import Control.Concurrent (MVar, newMVar)
import Control.Lens.Setter (set)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (
    MonadCatch,
    MonadThrow,
 )
import Control.Monad.Except (
    ExceptT (..),
    MonadError (..),
    runExceptT,
 )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.AWS (
    LogLevel (..),
    Logger,
    Region (..),
    envLogger,
    envRegion,
 )
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON (..), withText)
import Data.ByteString.Builder (toLazyByteString)
import Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)
import Database.SQLite.Simple (
    Connection,
    open,
 )
import GHC.Generics
import Network.AWS.Auth (
    AccessKey (..),
    Credentials (FromSession),
    SecretKey (..),
    SessionToken (..),
 )
import Network.AWS.Data.ByteString (toBS)
import Network.AWS.EC2.Types (InstanceType (..))
import qualified Network.AWS.Env as AWS
import qualified System.Logger as TL
import Text.Read (readMaybe)

data Opts = Opts
    { awsOpts :: AwsOpts
    , host :: String
    , port :: Word16
    , minInstances :: Word16
    , maxInstances :: Word16
    , dbPath :: FilePath
    , logLevel :: TL.Level
    }
    deriving (Show, Generic)

instance FromJSON Opts

data AwsOpts = AwsOpts
    { awsRegion :: Region
    , accessKey :: Text
    , secretKey :: Text
    , sessionToken :: Text
    , ec2Opts :: EC2Opts
    }
    deriving (Show, Generic)

instance FromJSON AwsOpts

data EC2Opts = EC2Opts
    { amiId :: Text
    , securityGroups :: [Text]
    , instanceType :: InstanceType
    , namePrefix :: Text
    , keypair :: Text
    , subnetId :: Text
    }
    deriving (Show, Generic)

instance FromJSON EC2Opts

instance FromJSON TL.Level where
    parseJSON = withText "Level" $ \tx -> case readMaybe . T.unpack $ tx of
        Just tp -> pure tp
        Nothing -> fail "Invalid log Level. Valid values: Trace, Debug, Info, Error"

instance FromJSON InstanceType where
    parseJSON = withText "InstanceType" $ \tx -> case readMaybe . T.unpack $ tx of
        Just tp -> pure tp
        Nothing -> fail "Invalid EC2 instance type"

type InstanceId = Text

data InstanceInfo = InstanceInfo
    { privateIp :: Maybe Text
    , instanceId :: InstanceId
    }
    deriving (Show)

data Env = Env
    { dbConn :: Connection
    , awsEnv :: AWS.Env
    , ec2Conf :: EC2Opts
    , monitors :: MVar (HashMap InstanceId InstanceInfo)
    , appLogger :: TL.Logger
    , appLogLevel :: TL.Level
    }

type ASGAction = ReaderT Env IO

newtype ASGActionE a = ASGActionE
    { unASGActionE :: ExceptT Text ASGAction a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadBase IO
        , MonadBaseControl IO
        , MonadThrow
        , MonadCatch
        , MonadError Text
        )

runASGAction :: Env -> ASGAction a -> IO a
runASGAction = flip runReaderT

mkEnv :: Opts -> IO Env
mkEnv opts = do
    logger <- mkLogger $ logLevel opts
    env <- mkAWSEnv (awsOpts opts) (TL.clone (Just "aws_logger") logger)
    monitorsVar <- newMVar HM.empty
    conn <- open $ dbPath opts
    return
        Env
            { dbConn = conn
            , awsEnv = env
            , ec2Conf = ec2Opts $ awsOpts opts
            , appLogger = logger
            , appLogLevel = logLevel opts
            , monitors = monitorsVar
            }
  where
    mkLogger :: TL.Level -> IO TL.Logger
    mkLogger lvl = TL.new . TL.setReadEnvironment False . TL.setLogLevel lvl $ TL.defSettings
    --
    mkAWSEnv :: AwsOpts -> TL.Logger -> IO AWS.Env
    mkAWSEnv o logger = do
        env <-
            AWS.newEnv $
                FromSession
                    (AccessKey $ toBS $ accessKey o)
                    (SecretKey $ toBS $ secretKey o)
                    (SessionToken $ toBS $ sessionToken o)
        return $ set envLogger (toAWSLogger logger) . set envRegion (awsRegion o) $ env
      where
        toAWSLogger :: TL.Logger -> Logger
        toAWSLogger lg lvl builder = TL.log lg (fromAWSLevel lvl) (TL.msg $ toLazyByteString builder)
        --
        fromAWSLevel :: LogLevel -> TL.Level
        fromAWSLevel Info = TL.Info
        fromAWSLevel Error = TL.Error
        fromAWSLevel Debug = TL.Debug
        fromAWSLevel Trace = TL.Trace

--
-- Error handling and logging
--
actionE :: ASGActionE a -> ASGAction a
actionE m = runExceptT (unASGActionE m) >>= either failWith pure
  where
    failWith errMsg = logErrText errMsg >> actionE m

logText :: (MonadIO m, MonadReader Env m) => Text -> m ()
logText t = do
    lg <- asks appLogger
    ll <- asks appLogLevel
    TL.log lg ll (TL.msg t)

logErrText :: (MonadIO m, MonadReader Env m) => Text -> m ()
logErrText t = asks appLogger >>= (\logger -> TL.err logger (TL.msg t))
