module Relay.Utils where

import Data.Proxy (Proxy)

class Example a where
  example :: Proxy a -> a
