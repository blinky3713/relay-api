{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Text as T
import Relay.Types (ExchangeOrder, OrderBook, TokenPair)
import Servant.API
import Servant.Client

type Paginated route = QueryParam "per_page" Int :> QueryParam "page" Int :> route

type GetExchangeOrders =
     "orders"
  :> QueryParams "exchangeContractAddress" T.Text
  :> QueryParams "tokenAddress" T.Text
  :> QueryParams "makerTokenAddress" T.Text
  :> QueryParams "takerTokenAddress" T.Text
  :> QueryParams "maker" T.Text
  :> QueryParams "taker" T.Text
  :> QueryParams "trader" T.Text
  :> QueryParams "feeRecipient" T.Text
  :> Get '[JSON] [ExchangeOrder]

getExchangeOrders ::
     Maybe Int
  -> Maybe Int
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> ClientM [ExchangeOrder]
getExchangeOrders = client $ Proxy @(Paginated GetExchangeOrders)

type GetOrderByHash =
    "order"
  :> Capture "hash" T.Text
  :> Get '[JSON] ExchangeOrder

getOrderByHash
  :: T.Text
  -> ClientM ExchangeOrder
getOrderByHash = client $ Proxy @GetOrderByHash

type GetOrderBook =
      "orderbook"
   :> QueryParams "baseTokenAddress" T.Text
   :> QueryParams "quoteTokenAddress" T.Text
   :> Get '[JSON] OrderBook

getOrderBook
  :: Maybe Int
  -> Maybe Int
  -> [T.Text]
  -> [T.Text]
  -> ClientM OrderBook
getOrderBook = client $ Proxy @(Paginated GetOrderBook)

type GetTokenPairs =
     "token_pairs"
  :> QueryParams "tokenA=&tokenB" T.Text
  :> Get '[JSON] [TokenPair]

getTokenPairs
  :: [T.Text]
  -> ClientM [TokenPair]
getTokenPairs = client $ Proxy @GetTokenPairs
