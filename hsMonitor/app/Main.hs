{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, replicateM_)

import Env (Env (..), mkEnv)
import Grpc.Client (pushMetrics, runActions)
import MetricGen (genRandomVar, genSeed, sampleLoad)

tenSecs :: Int
tenSecs = 10000000

main :: IO ()
main = do
    env <- mkEnv
    runActions (asgHost env) (asgPort env) $ \client -> forever $ do
        cpuLoad <- sampleLoad (seed env) (rvar env)
        httpLoad <- sampleLoad (seed env) (rvar env)
        (pushMetrics cpuLoad httpLoad undefined 5 client)
        threadDelay tenSecs
