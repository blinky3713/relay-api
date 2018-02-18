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
    , mkClientApp
    ) where

import Control.Monad (forever)
import qualified Data.Aeson as A
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Network.WebSockets (runClient, receiveData, sendBinaryData, forkPingThread)
import Relay.Types (ExchangeOrder, OrderBook, TokenPair, WebsocketReqPayload, WebsocketReq(..), WebsocketResponse(..))
import Servant.API
import Servant.Client
import Wuss (runSecureClient)

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

-- Websocket

mkClientApp
  :: WebsocketReqPayload
  -> (OrderBook -> IO Bool)
  -> IO ()
mkClientApp req handler = do
    let host = "ws.radarrelay.com"
        path = "/0x/v0/ws"
        port = 443
        wsReq = WebsocketReq { websocketreqChannel = "orderbook"
                             , websocketreqRequestId = 1
                             , websocketreqType = "subscribe"
                             , websocketreqPayload = req
                             }
    runSecureClient host port path $ \connection -> do
      let req = A.encode wsReq
      print req
      _ <- sendBinaryData connection req
      forkPingThread connection 10
      loop connection
  where
    loop conn = forever $ do
      raw <- receiveData conn
      print $ raw
      case websocketresponsePayload <$> A.decode raw of
        Nothing -> do
          print $  "Couldn't decode as OrderBook : " ++ show raw
          loop conn
        Just ob -> do
          continue <- handler ob
          if continue
            then loop conn
            else return ()
