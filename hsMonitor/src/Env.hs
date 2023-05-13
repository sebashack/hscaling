{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Env (mkEnv, Env (..)) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict')
import Data.Random (RVar)
import Data.Text as T
import GHC.Generics
import Shelly (run, shelly, silently)

import MetricGen (Seed, genRandomVar, genSeed)

data Opts = Opts
    { asgServerHost :: String
    , asgServerPort :: Int
    , samplingLambda :: Float
    , pushFrequencySecs :: Int
    }
    deriving (Show, Generic)

instance FromJSON Opts

data Env = Env
    { asgHost :: String
    , asgPort :: Int
    , privateDNSName :: Text
    , seed :: Seed
    , rvar :: RVar Int
    , pushFrequency :: Int
    }

mkEnv :: IO Env
mkEnv = do
    dnsName <- getPrivateDNSName
    eitherConf <- eitherDecodeFileStrict' "/opt/monitor_config.json"
    case eitherConf of
        Left _ -> error "Could not parse monitor_config.json"
        Right opts -> do
            s <- genSeed
            return $
                Env
                    { asgHost = asgServerHost opts
                    , asgPort = asgServerPort opts
                    , privateDNSName = dnsName
                    , seed = s
                    , rvar = genRandomVar $ samplingLambda opts
                    , pushFrequency = pushFrequencySecs opts
                    }
  where
    getPrivateDNSName :: IO Text
    getPrivateDNSName = shelly $ silently $ do
        out <- run "hostname" []
        return ((T.strip out) <> ".ec2.internal")
