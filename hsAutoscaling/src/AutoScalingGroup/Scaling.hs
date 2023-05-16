{-# LANGUAGE OverloadedStrings #-}

module AutoScalingGroup.Scaling (scaleAction, initializeInstances) where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (listToMaybe)
import Data.Text as T (pack)

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
    let f i (a1, a2, n) = (IM.cpuLoadPercentage i + a1, IM.httpLoadPercentage i + a2, n + 1)
        (sumCpuLoad, sumHttpLoad, k) = foldr f (0, 0, 0 :: Float) metricss
        avgCpuLoad = sumCpuLoad / k
        avgHttpLoad = sumHttpLoad / k
    logText
        ( ">>>>>>> scale: average cpu = "
            <> showT avgCpuLoad
            <> ", average http = "
            <> showT avgHttpLoad
        )
    scaleUpOrDown (listToMaybe metricss) avgCpuLoad avgHttpLoad
  where
    showT = T.pack . show

scaleUpOrDown :: Maybe InstanceMetrics -> Float -> Float -> ASGActionE ()
scaleUpOrDown maybeIns avgCpuLoad avgHttpLoad = do
    maybeMaxCpuLoad <- asks appCpuMaxLoadPercentage
    maybeMaxHttpLoad <- asks appHttpMaxLoadPercentage
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
            logText ">>>>>>> scale: scaling up ..."
        (False, True) -> do
            case maybeIns of
                Just m -> do
                    terminateInstance $ IM.instanceId m
                    liftIO $ deleteInstance conn (IM.instanceId m)
                    logText ">>>>>>> scale: scaling down ..."
                Nothing -> return ()
        (False, False) -> return ()
        (True, True) -> logText ">>>>>>> scale-WARNING: both scale-up and scale-down conditions are True"
  where
    shouldScaleUp :: Int -> Int -> Maybe Float -> Maybe Float -> Bool
    shouldScaleUp currentCount maxCount maybeMaxCpuLoad maybeMaxHttpLoad =
        if currentCount < maxCount
            then case (maybeMaxCpuLoad, maybeMaxHttpLoad) of
                (Nothing, Nothing) -> avgCpuLoad >= 60
                (Nothing, Just httpLoad) -> avgHttpLoad >= httpLoad
                (Just cpuLoad, Nothing) -> avgCpuLoad >= cpuLoad
                (Just cpuLoad, Just httpLoad) -> avgHttpLoad >= httpLoad || avgCpuLoad >= cpuLoad
            else False
    --
    shouldScaleDown :: Int -> Int -> Maybe Float -> Maybe Float -> Bool
    shouldScaleDown currentCount minCount maybeMaxCpuLoad maybeMaxHttpLoad =
        if currentCount > minCount
            then case (maybeMaxCpuLoad, maybeMaxHttpLoad) of
                (Nothing, Nothing) -> avgCpuLoad < 60
                (Nothing, Just httpLoad) -> avgHttpLoad < httpLoad
                (Just cpuLoad, Nothing) -> avgCpuLoad < cpuLoad
                (Just cpuLoad, Just httpLoad) -> avgHttpLoad < httpLoad && avgCpuLoad < cpuLoad
            else False
