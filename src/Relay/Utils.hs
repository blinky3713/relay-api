module Relay.Utils where

import Data.Proxy (Proxy)

class Example a where
  eg :: Proxy a -> a
