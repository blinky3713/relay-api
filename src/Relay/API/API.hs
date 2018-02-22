{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Relay.API.API
    ( GetExchangeOrders
    , GetOrderByHash
    , GetOrderBook
    , GetTokenPairs
    , RelayAPI
    , relayAPI
    , SwaggerAPI
    , relaySwagger
    , Paginated
    ) where

import Data.Proxy (Proxy(..))
import Relay.Types
import Servant.API
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

type GetOrderByHash =
    "order"
  :> Capture "hash" HexString
  :> Get '[JSON] ExchangeOrder

type GetOrderBook =
      "orderbook"
   :> QueryParams "baseTokenAddress" Address
   :> QueryParams "quoteTokenAddress" Address
   :> Get '[JSON] OrderBook

type GetTokenPairs =
     "token_pairs"
  :> QueryParams "tokenA=&tokenB" Address
  :> Get '[JSON] [TokenPair]

type RelayAPI =
       GetExchangeOrders
  :<|> GetExchangeOrders
  :<|> GetOrderByHash
  :<|> GetOrderBook
  :<|> GetTokenPairs

relayAPI :: Proxy RelayAPI
relayAPI = Proxy

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

relaySwagger :: Swagger
relaySwagger = toSwagger relayAPI
  & info.title   .~ "Relay API"
  & info.version .~ "v0"
