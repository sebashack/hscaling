{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AutoScalingGroup.CRUD (
    closeConn,
    deleteInstance,
    insertInstance,
    insertMetric,
    insertPing,
) where

import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    close,
    execute,
    query,
    query_,
 )
import Text.RawString.QQ

import AutoScalingGroup.TYPES (InstanceMetrics)

insertInstance :: Connection -> Text -> Text -> Text -> IO ()
insertInstance conn insId privateIp privateDNSName = do
    now <- getCurrentTime
    execute conn q (insId, privateIp, privateDNSName, now)
  where
    q =
        [r|
        INSERT INTO instance (id, private_ip, private_dns_name, created_at)
        VALUES (?, ?, ?, ?)
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

insertPing :: Connection -> Text -> IO ()
insertPing conn insId = do
    now <- getCurrentTime
    execute conn q (insId, now)
  where
    q =
        [r|
        INSERT INTO ping (instance_id, created_at)
        VALUES (?, ?)
        |]

deleteInstance :: Connection -> Text -> IO ()
deleteInstance conn insId = execute conn "DELETE FROM instance WHERE id=?" (Only insId)

closeConn :: Connection -> IO ()
closeConn = close

selectInstancesMetrics :: Connection -> IO [InstanceMetrics]
selectInstancesMetrics conn = do
  query_ conn q
  where
    q = "SELECT instc.id, met.cpu_load_percentage, met.http_load_percentage, met.created_at \
        \FROM instance instc \
        \INNER JOIN metric met ON instc.id = met.instance_id \
        \WHERE met.created_at = ( \
        \SELECT MAX(created_at) \
        \FROM metric \
        \WHERE instance_id = instc.id)"
