data InstanceMetrics = InstanceMetrics
  { instanceId :: String
  , cpuLoadPercentage :: Double
  , httpLoadPercentage :: Double
  }
  deriving (Eq, Show, Generic)

instance FromRow InstanceMetrics where
  fromRow = InstanceMetrics <$> field <*> field <*> field
