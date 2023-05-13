{-# LANGUAGE OverloadedStrings #-}

module Env (mkEnv, Env (..)) where

import Control.Monad (void)
import Data.Random (RVar)
import Data.Text (Text)
import Data.Text as T
import Data.Text.Read (decimal)
import Shelly (lastExitCode, run, shelly, silently)

import MetricGen (Seed, genRandomVar, genSeed)

data Env = Env
    { asgHost :: String
    , asgPort :: Int
    , privateDNSName :: Text
    , seed :: Seed
    , rvar :: RVar Int
    }

mkEnv :: IO Env
mkEnv = do
    dnsName <- getPrivateDNSName
    (host, port) <- getMonitorHostAndPort
    s <- genSeed
    return
        Env
            { asgHost = T.unpack host
            , asgPort = port
            , privateDNSName = dnsName
            , seed = s
            , rvar = genRandomVar
            }
  where
    getPrivateDNSName :: IO Text
    getPrivateDNSName = shelly $ silently $ do
        out <- run "hostname" []
        return ((T.strip out) <> ".ec2.internal")

    getMonitorHostAndPort :: IO (Text, Int)
    getMonitorHostAndPort = shelly $ silently $ do
        out <- run "cat" ["/opt/asg_server"]
        let host : port : _ = T.splitOn ":" out
        case decimal port of
            Left err -> error err
            Right (p, _) -> return (host, p)
