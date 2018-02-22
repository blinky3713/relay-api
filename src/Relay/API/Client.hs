{-# LANGUAGE TypeApplications #-}

module Relay.API.Client
    ( getExchangeOrders
    , getOrderByHash
    , getOrderBook
    , getTokenPairs
    , mkRelayClientEnv
    ) where

import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Relay.API.API
import Relay.Types
import Servant.Client

getExchangeOrders ::
     Maybe Int
  -> Maybe Int
  -> [Address]
  -> [Address]
  -> [Address]
  -> [Address]
  -> [Address]
  -> [Address]
  -> [Address]
  -> [Address]
  -> ClientM [ExchangeOrder]
getExchangeOrders = client $ Proxy @(Paginated GetExchangeOrders)

getOrderByHash
  :: HexString
  -> ClientM ExchangeOrder
getOrderByHash = client $ Proxy @GetOrderByHash

getOrderBook
  :: Maybe Int
  -> Maybe Int
  -> [Address]
  -> [Address]
  -> ClientM OrderBook
getOrderBook = client $ Proxy @(Paginated GetOrderBook)

getTokenPairs
  :: [Address]
  -> ClientM [TokenPair]
getTokenPairs = client $ Proxy @GetTokenPairs

mkRelayClientEnv
  :: String
  -> String
  -> Int
  -> IO ClientEnv
mkRelayClientEnv hst pth prt = do
  mgr <- newManager defaultManagerSettings
  return $ ClientEnv mgr (BaseUrl Http hst prt pth)
