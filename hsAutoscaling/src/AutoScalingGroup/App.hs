module AutoScalingGroup.App (runApp) where

import AutoScalingGroup.Env (Env (..), actionE, runASGAction)
import AutoScalingGroup.Ping (pingAction)
import AutoScalingGroup.Scaling (initializeInstances, scaleAction)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Grpc.Server (runServer)

runApp :: Env -> IO ()
runApp env = do
    void $ (runASGAction env $ actionE initializeInstances)
    void $ forkIO (runASGAction env $ actionE $ forever pingAction)
    void $ forkIO (runASGAction env $ actionE $ forever scaleAction)
    runServer (dbConn env) (appGrpcHost env) (appGrpcPort env)
