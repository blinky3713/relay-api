module Relay
  ( module Relay.Types
  , module Relay.API
  , mkRelayClientEnv
  ) where

import Network.HTTP.Client
import Relay.Types
import Relay.API
import Servant.Client

mkRelayClientEnv
  :: String
  -> String
  -> Int
  -> IO ClientEnv
mkRelayClientEnv host path port = do
  mgr <- newManager defaultManagerSettings
  return $ ClientEnv mgr (BaseUrl Http host port path)
