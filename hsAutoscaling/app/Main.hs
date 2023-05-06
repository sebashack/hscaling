module Main (main) where

import Grpc.Server (runServer)

main :: IO ()
main = do
    putStrLn "Running on 127.0.0.1:50001"
    runServer "127.0.0.1" 50001

