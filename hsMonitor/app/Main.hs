{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Env (Env (..), mkEnv)
import Grpc.Client (MonitorResponse (..), pushMetrics, runActions)
import MetricGen (sampleLoad)

main :: IO ()
main = do
    env <- mkEnv
    putStrLn ">>>>>>> running monitor ..."
    runActions (asgHost env) (asgPort env) $ \client -> forever $ do
        cpuLoad <- sampleLoad (seed env) (rvar env)
        httpLoad <- sampleLoad (seed env) (rvar env)
        putStrLn (">>>>>>> http-load = " <> show httpLoad)
        putStrLn (">>>>>>> cpu-load = " <> show cpuLoad)
        putStrLn ">>>>>>>"
        res <- pushMetrics cpuLoad httpLoad (privateDNSName env) 5 client
        case res of
            OkResponse _ -> putStrLn "ok response"
            ErrResponse err -> putStrLn ("error response: " <> err)
        threadDelay (pushFrequency env * 1000000)
