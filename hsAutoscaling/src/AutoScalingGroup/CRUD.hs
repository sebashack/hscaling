{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AutoScalingGroup.CRUD where

import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    close,
    execute,
    query,
    query_,
 )
import Text.RawString.QQ

insertInstance :: Connection -> Text -> Text -> IO ()
insertInstance conn insId privateId = do
    now <- getCurrentTime
    execute conn q (insId, privateId, now)
  where
    q =
        [r|
        INSERT INTO instance (id, private_ip, created_at)
        VALUES (?, ?, ?)
        |]

insertMetric :: Connection -> Text -> Float -> Float -> IO ()
insertMetric conn insId cpuLoad httpLoad = do
    now <- getCurrentTime
    execute conn q (insId, cpuLoad, httpLoad, now)
  where
    q =
        [r|
        INSERT INTO metric (instance_id, cpu_load_percentage, http_load_percentage, created_at)
        VALUES (?, ?, ?, ?)
        |]
