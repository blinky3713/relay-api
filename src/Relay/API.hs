{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Relay.API
    ( GetExchangeOrders
    , getExchangeOrders
    , GetOrderByHash
    , getOrderByHash
    , GetOrderBook
    , getOrderBook
    , GetTokenPairs
    , getTokenPairs
    ) where

import Data.Proxy (Proxy(..))
import Relay.Types
import Servant.API
import Servant.Client
import Control.Lens ((&), (.~))
import Data.Swagger (Swagger, info, title, version)
import           Servant.Swagger


type Paginated route = QueryParam "per_page" Int :> QueryParam "page" Int :> route

type GetExchangeOrders =
     "orders"
  :> QueryParams "exchangeContractAddress" Address
  :> QueryParams "tokenAddress" Address
  :> QueryParams "makerTokenAddress" Address
  :> QueryParams "takerTokenAddress" Address
  :> QueryParams "maker" Address
  :> QueryParams "taker" Address
  :> QueryParams "trader" Address
  :> QueryParams "feeRecipient" Address
  :> Get '[JSON] [ExchangeOrder]

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

type GetOrderByHash =
    "order"
  :> Capture "hash" HexString
  :> Get '[JSON] ExchangeOrder

getOrderByHash
  :: HexString
  -> ClientM ExchangeOrder
getOrderByHash = client $ Proxy @GetOrderByHash

type GetOrderBook =
      "orderbook"
   :> QueryParams "baseTokenAddress" Address
   :> QueryParams "quoteTokenAddress" Address
   :> Get '[JSON] OrderBook

getOrderBook
  :: Maybe Int
  -> Maybe Int
  -> [Address]
  -> [Address]
  -> ClientM OrderBook
getOrderBook = client $ Proxy @(Paginated GetOrderBook)

type GetTokenPairs =
     "token_pairs"
  :> QueryParams "tokenA=&tokenB" Address
  :> Get '[JSON] [TokenPair]

getTokenPairs
  :: [Address]
  -> ClientM [TokenPair]
getTokenPairs = client $ Proxy @GetTokenPairs

type RelayAPI =
       GetExchangeOrders
  :<|> GetExchangeOrders
  :<|> GetOrderByHash
  :<|> GetOrderBook
  :<|> GetTokenPairs

relayAPI :: Proxy RelayAPI
relayAPI = Proxy

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

relaySwagger :: Swagger
relaySwagger = toSwagger relayAPI
  & info.title   .~ "Relay API"
  & info.version .~ "v0"
