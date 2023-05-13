{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AutoScalingGroup.CRUD (
    closeConn,
    deleteInstance,
    insertInstance,
    insertMetric,
    selectInstanceByDNSName,
    selectInstanceCount,
    selectInstanceMetrics,
    enableForeignKeys,
    Instance (..),
    InstanceMetrics (..),
    selectInstances,
) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple (
    Connection,
    FromRow (..),
    Only (..),
    close,
    execute,
    execute_,
    field,
    query,
    query_,
 )
import Text.RawString.QQ

data InstanceMetrics = InstanceMetrics
    { instanceId :: Text
    , cpuLoadPercentage :: Float
    , httpLoadPercentage :: Float
    , createdAt :: UTCTime
    }
    deriving (Eq, Show)

instance FromRow InstanceMetrics where
    fromRow = InstanceMetrics <$> field <*> field <*> field <*> field

data Instance = Instance
    { instanceId :: Text
    , privateIp :: Text
    , privateDNSName :: Text
    , createdAt :: UTCTime
    }
    deriving (Eq, Show)

instance FromRow Instance where
    fromRow = Instance <$> field <*> field <*> field <*> field

insertInstance :: Connection -> Text -> Text -> Text -> IO ()
insertInstance conn insId privIp dnsName = do
    now <- getCurrentTime
    execute conn q (insId, privIp, dnsName, now)
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

deleteInstance :: Connection -> Text -> IO ()
deleteInstance conn insId = execute conn "DELETE FROM instance WHERE id=?" (Only insId)

selectInstanceMetrics :: Connection -> IO [InstanceMetrics]
selectInstanceMetrics conn = query_ conn q
  where
    q =
        [r|
        SELECT i.id, m.cpu_load_percentage, m.http_load_percentage, m.created_at
        FROM instance i
        INNER JOIN metric m ON i.id = m.instance_id
        WHERE m.created_at = (SELECT MAX(created_at) FROM metric WHERE instance_id = i.id)
        |]

selectInstanceCount :: Connection -> IO Int
selectInstanceCount conn = do
    result <- query_ conn "SELECT COUNT(id) FROM instance"
    case listToMaybe result of
        Just (Only v) -> return v
        Nothing -> return 0

selectInstanceByDNSName :: Connection -> Text -> IO (Maybe Instance)
selectInstanceByDNSName conn dnsName = do
    result <- query conn q (Only dnsName)
    pure $ listToMaybe result
  where
    q =
        [r|
        SELECT id, private_ip, private_dns_name, created_at FROM instance WHERE private_dns_name = ?
        |]

selectInstances :: Connection -> IO [Instance]
selectInstances conn = query_ conn "SELECT id, private_ip, private_dns_name, created_at FROM instance"

enableForeignKeys :: Connection -> IO ()
enableForeignKeys conn = execute_ conn "PRAGMA foreign_keys=ON"

closeConn :: Connection -> IO ()
closeConn = close
