module AutoScalingGroup.App (runApp) where

import AutoScalingGroup.Env (
    Env (..),
    PingOpts (pingFrequencySecs),
    actionE,
    runASGAction,
 )
import AutoScalingGroup.Ping (pingAction)
import AutoScalingGroup.Scaling (initializeInstances, scaleAction)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Grpc.Server (runServer)

runApp :: Env -> IO ()
runApp env = do
    let pingDelay = pingFrequencySecs (pingConf env) * 1000000
        scaleDelay = appBalancingFrequency env * 1000000
        pingAction' = liftIO (threadDelay pingDelay) >> pingAction
        scaleAction' = liftIO (threadDelay scaleDelay) >> scaleAction
    void $ (runASGAction env $ actionE initializeInstances)
    putStrLn (">>>>>>>>> waiting " <> show (appInitDelay env) <> " sec(s) for instances to intiailize ...")
    threadDelay (appInitDelay env * 1000000)
    void $ forkIO (runASGAction env $ actionE $ forever pingAction')
    void $ forkIO (runASGAction env $ actionE $ forever scaleAction')
    runServer (dbConn env) (appGrpcHost env) (appGrpcPort env)
