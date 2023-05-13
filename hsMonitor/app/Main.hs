{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Grpc.Client (pushMetrics, runActions)

main :: IO ()
main = do
    res <- runActions "127.0.0.1" 50001 (pushMetrics 0.5 0.5 "currywurst" 5)
    print res
