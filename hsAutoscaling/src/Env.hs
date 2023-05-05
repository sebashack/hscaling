{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Env (Opts (..), Env (..), mkEnv) where

import Control.Concurrent (MVar, newMVar)
import Control.Lens.Setter (set)
import Control.Monad.Trans.AWS (
    LogLevel (..),
    Logger,
    Region (..),
    envLogger,
    envRegion,
 )
import Data.Aeson (
    FromJSON (..),
    withText,
 )
import Data.ByteString.Builder (toLazyByteString)
import Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)
import GHC.Generics
import Network.AWS.Auth (
    AccessKey (..),
    Credentials (FromSession),
    SecretKey (..),
    SessionToken (..),
 )
import Network.AWS.Data.ByteString (toBS)
import qualified Network.AWS.Env as AWS
import qualified System.Logger as TL
import Text.Read (readMaybe)

data Opts = Opts
    { awsOpts :: AwsOpts
    , clientPort :: Word16
    , clientRequestTimeoutSecs :: Int
    , logLevel :: TL.Level
    }
    deriving (Show, Generic)

data AwsOpts = AwsOpts
    { awsRegion :: Region
    , accessKey :: Text
    , secretKey :: Text
    , sessionToken :: Text
    }
    deriving (Show, Generic)

instance FromJSON TL.Level where
    parseJSON = withText "Level" $ \lvl -> case readMaybe . T.unpack $ lvl of
        Just lv -> pure lv
        Nothing -> fail "Invalid log Level. Valid values: Trace, Debug, Info, Error"

data Env = Env
    { awsEnv :: AWS.Env
    , monitors :: MVar (Set Text)
    , appLogger :: TL.Logger
    }

mkEnv :: Opts -> IO Env
mkEnv opts = do
    logger <- mkLogger $ logLevel opts
    env <- mkAWSEnv (awsOpts opts) (TL.clone (Just "aws_logger") logger)
    monitorsVar <- newMVar Set.empty
    return Env{awsEnv = env, appLogger = logger, monitors = monitorsVar}
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
