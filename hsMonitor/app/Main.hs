{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)

import Env (Env (..), mkEnv)
import Grpc.Client (pushMetrics, runActions)
import MetricGen (sampleLoad)

main :: IO ()
main = do
    env <- mkEnv
    runActions (asgHost env) (asgPort env) $ \client -> forever $ do
        cpuLoad <- sampleLoad (seed env) (rvar env)
        httpLoad <- sampleLoad (seed env) (rvar env)
        void $ pushMetrics cpuLoad httpLoad (privateDNSName env) 5 client
        threadDelay (pushFrequency env * 1000000)
