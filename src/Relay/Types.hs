{-# LANGUAGE DeriveGeneric #-}

module Relay.Types
  ( ExchangeOrder(..)
  , OrderBook(..)
  , relayOptions
  ) where


import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.Text as T
import GHC.Generics

data ExchangeOrder =
  ExchangeOrder { exchangeOrderExchangeContractAddress :: T.Text
                , exchangeOrderMaker :: T.Text
                , exchangeOrderTaker :: T.Text
                , exchangeOrderMakerTokenAddress :: T.Text
                , exchangeOrderTakerTokenAddress :: T.Text
                , exchangeOrderMakerFee :: Integer
                , exchangeOrderTakerFee :: Integer
                , exchangeOrderExpirationUnixTimestamp :: Integer
                }
  deriving (Eq, Show, Generic)

instance A.FromJSON ExchangeOrder where
  parseJSON = A.genericParseJSON relayOptions


data OrderBook =
  OrderBook { orderBookAsks :: [ExchangeOrder]
            , orderBookBids :: [ExchangeOrder]
            }
  deriving (Eq, Show, Generic)

instance A.FromJSON OrderBook where
  parseJSON = A.genericParseJSON relayOptions



-- JSON Options

relayOptions :: A.Options
relayOptions = aesonPrefix camelCase
