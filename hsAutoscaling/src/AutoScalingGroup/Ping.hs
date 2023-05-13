{-# LANGUAGE OverloadedStrings #-}

module AutoScalingGroup.Ping (pingAction) where

import Control.Concurrent.Async.Lifted (mapConcurrently_)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text as T
import Data.Word (Word8)
import Shelly (errExit, lastExitCode, run, shelly, silently)

import AutoScalingGroup.AWS (runInstance, terminateInstance)
import AutoScalingGroup.AWS as INF (InstanceInfo (..))
import AutoScalingGroup.CRUD (
    Instance,
    deleteInstance,
    insertInstance,
    selectInstanceCount,
    selectInstances,
 )
import AutoScalingGroup.CRUD as IN (Instance (..))
import AutoScalingGroup.Env (ASGActionE, Env (..), PingOpts (..))

pingAction :: ASGActionE ()
pingAction = do
    conn <- asks dbConn
    instances <- liftIO $ selectInstances conn
    mapConcurrently_ pingOrRun instances

--
-- Helpers
--
pingOrRun :: Instance -> ASGActionE ()
pingOrRun ins = do
    timeout <- asks (responseTimeoutSecs . pingConf)
    count' <- asks (responseCount . pingConf)
    isAlive <- liftIO $ ping (IN.privateIp ins) timeout count'
    if isAlive
        then return ()
        else do
            terminateInstance $ IN.instanceId ins
            conn <- asks dbConn
            currentCount <- liftIO $ selectInstanceCount conn
            minCount <- asks appMinInstances
            if currentCount - 1 < fromIntegral minCount
                then do
                    info <- runInstance
                    liftIO $ insertInstance conn (INF.instanceId info) (INF.privateIp info) (INF.privateDNSName info)
                else do pure ()
            liftIO $ deleteInstance conn (IN.instanceId ins)
  where
    ping :: Text -> Word8 -> Word8 -> IO Bool
    ping host resTimeout resCount = shelly $ silently $ do
        let cmd = run "ping" ["-W", T.pack $ show resTimeout, "-c", T.pack $ show resCount, host]
        void $ errExit False cmd
        code <- lastExitCode
        return $ code == 0
