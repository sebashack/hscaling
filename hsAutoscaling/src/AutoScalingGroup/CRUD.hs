{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AutoScalingGroup.CRUD (
    closeConn,
    deleteInstance,
    insertInstance,
    insertMetric,
    insertPing,
    selectInstanceMetrics,
) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple (
    Connection,
    FromRow (..),
    Only (..),
    close,
    execute,
    field,
    query,
    query_,
 )
import Text.RawString.QQ

data InstanceMetrics = InstanceMetrics
    { instanceId :: Text
    , cpuLoadPercentage :: Double
    , httpLoadPercentage :: Double
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
    fromRow = InstanceMetrics <$> field <*> field <*> field <*> field

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

selectInstanceMetrics :: Connection -> IO [InstanceMetrics]
selectInstanceMetrics conn = do
    query_ conn q
  where
    q =
        [r|
        SELECT i.id, m.cpu_load_percentage, m.http_load_percentage, m.created_at
        FROM instance i
        INNER JOIN metric m ON i.id = m.instance_id
        WHERE m.created_at = (SELECT MAX(created_at) FROM metric WHERE instance_id = i.id)
        |]

selectIntancesCount :: Connection -> Text -> IO Int
selectInstanceCount conn = query_ conn "SELECT COUNT(id) FROM instance"

selectInstance :: Connection -> IO [Instance]
selectInstance conn privateDNSName = do
    query_ conn q privateDNSName
  where
    q =
        [r| 
	SELECT id, private_ip, private_dns_name, created_at FROM instance WHERE private_dns_name = ?
        |]

closeConn :: Connection -> IO ()
closeConn = close
