module Main (main) where

import Grpc.Client (getHeartbeat, runActions)

main :: IO ()
main = do
    res <- runActions "127.0.0.1" 50001 getHeartbeat
    print res
