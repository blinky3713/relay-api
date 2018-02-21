module Relay
  ( module Relay.Types
  , module Relay.API
  , module Servant.Client
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
mkRelayClientEnv hst pth prt = do
  mgr <- newManager defaultManagerSettings
  return $ ClientEnv mgr (BaseUrl Http hst prt pth)
