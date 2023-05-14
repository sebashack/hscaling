{-# LANGUAGE OverloadedStrings #-}

module AutoScalingGroup.Scaling (scaleAction, initializeInstances) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import AutoScalingGroup.AWS (runInstance, terminateInstance)
import AutoScalingGroup.AWS as INF (InstanceInfo (..))
import AutoScalingGroup.CRUD (
    deleteInstance,
    insertInstance,
    selectInstanceCount,
    selectInstanceMetrics,
 )
import AutoScalingGroup.CRUD as IM (InstanceMetrics (..))
import AutoScalingGroup.Env (ASGActionE, Env (..), logText)

initializeInstances :: ASGActionE ()
initializeInstances = do
    conn <- asks dbConn
    minCount <- asks appMinInstances
    currentCount <- liftIO $ selectInstanceCount conn
    let diff = (fromIntegral minCount) - currentCount
    if diff > 0
        then replicateM_ diff $ do
            info <- runInstance
            liftIO $ insertInstance conn (INF.instanceId info) (INF.privateIp info) (INF.privateDNSName info)
        else return ()

scaleAction :: ASGActionE ()
scaleAction = do
    conn <- asks dbConn
    metricss <- liftIO $ selectInstanceMetrics conn
    mapM_ scaleDownOrUp metricss

scaleDownOrUp :: InstanceMetrics -> ASGActionE ()
scaleDownOrUp m = do
    maybeMaxCpuLoad <- asks appHttpMaxLoadPercentage
    maybeMaxHttpLoad <- asks appCpuMaxLoadPercentage
    minCount <- asks appMinInstances
    maxCount <- asks appMaxInstances
    conn <- asks dbConn
    currentCount <- liftIO $ selectInstanceCount conn
    let shouldScaleUpB = shouldScaleUp currentCount (fromIntegral maxCount) maybeMaxCpuLoad maybeMaxHttpLoad
        shouldScaleDownB = shouldScaleDown currentCount (fromIntegral minCount) maybeMaxCpuLoad maybeMaxHttpLoad
    case (shouldScaleUpB, shouldScaleDownB) of
        (True, False) -> do
            info <- runInstance
            liftIO $ insertInstance conn (INF.instanceId info) (INF.privateIp info) (INF.privateDNSName info)
            initDelay <- asks appInitDelay
            liftIO $ threadDelay (initDelay * 1000000)
        (False, True) -> do
            terminateInstance $ IM.instanceId m
            liftIO $ deleteInstance conn (IM.instanceId m)
        (False, False) -> return ()
        (True, True) -> logText "WARNING: both scale-up and scale-down conditions are True"
  where
    shouldScaleUp :: Int -> Int -> Maybe Float -> Maybe Float -> Bool
    shouldScaleUp currentCount maxCount maybeMaxCpuLoad maybeMaxHttpLoad =
        if currentCount < maxCount
            then case (maybeMaxCpuLoad, maybeMaxHttpLoad) of
                (Nothing, Nothing) -> cpuLoadPercentage m >= 60
                (Nothing, Just httpLoad) -> httpLoadPercentage m >= httpLoad
                (Just cpuLoad, Nothing) -> cpuLoadPercentage m >= cpuLoad
                (Just cpuLoad, Just httpLoad) -> httpLoadPercentage m >= httpLoad || cpuLoadPercentage m >= cpuLoad
            else False
    --
    shouldScaleDown :: Int -> Int -> Maybe Float -> Maybe Float -> Bool
    shouldScaleDown currentCount minCount maybeMaxCpuLoad maybeMaxHttpLoad =
        if currentCount > minCount
            then case (maybeMaxCpuLoad, maybeMaxHttpLoad) of
                (Nothing, Nothing) -> cpuLoadPercentage m < 60
                (Nothing, Just httpLoad) -> httpLoadPercentage m < httpLoad
                (Just cpuLoad, Nothing) -> cpuLoadPercentage m < cpuLoad
                (Just cpuLoad, Just httpLoad) -> httpLoadPercentage m < httpLoad && cpuLoadPercentage m < cpuLoad
            else False
