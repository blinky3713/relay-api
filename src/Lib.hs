{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Types (ECSignature, ExchangeOrder)
import Servant.API
import Servant.Client

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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
     [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> [T.Text]
  -> ClientM [ExchangeOrder]
getExchangeOrders = client $ Proxy @GetExchangeOrders

type GetOrder =
    "order"
  :> Capture "hash" T.Text
  :> Get '[JSON] ExchangeOrder

getOrderByHash
  :: T.Text
  -> ClientM ExchangeOrder
getOrderByHash = client $ Proxy @GetOrder

