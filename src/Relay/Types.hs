{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Relay.Types
  ( ExchangeOrder(..)
  , OrderBook(..)
  , Token(..)
  , TokenPair(..)
  , WebsocketReq(..)
  ) where


import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.Text as T
import GHC.Generics

data ExchangeOrder =
  ExchangeOrder { exchangeorderExchangeContractAddress :: T.Text
                , exchangeorderMaker :: T.Text
                , exchangeorderTaker :: T.Text
                , exchangeorderMakerTokenAddress :: T.Text
                , exchangeorderTakerTokenAddress :: T.Text
                , exchangeorderMakerFee :: T.Text
                , exchangeorderTakerFee :: T.Text
                , exchangeorderExpirationUnixTimestampSec :: T.Text
                }
  deriving (Eq, Show, Generic)

instance A.FromJSON ExchangeOrder where
  parseJSON = A.genericParseJSON relayOptions

instance A.ToJSON ExchangeOrder where
  toJSON = A.genericToJSON relayOptions


data OrderBook =
  OrderBook { orderbookAsks :: [ExchangeOrder]
            , orderbookBids :: [ExchangeOrder]
            }
  deriving (Eq, Show, Generic)

instance A.FromJSON OrderBook where
  parseJSON = A.genericParseJSON relayOptions

instance A.ToJSON OrderBook where
  toJSON = A.genericToJSON relayOptions

data Token =
  Token { tokenAddress :: T.Text
        , tokenMinAmount :: T.Text
        , tokenMaxAmount :: T.Text
        , tokenPrecision :: Int
        }
  deriving (Eq, Show, Generic)

instance A.FromJSON Token where
  parseJSON = A.genericParseJSON relayOptions

instance A.ToJSON Token where
  toJSON = A.genericToJSON relayOptions

data TokenPair =
  TokenPair { tokenpairTokenA :: Token
            , tokenpairTokenB :: Token
            }
  deriving (Eq, Show, Generic)

instance A.FromJSON TokenPair where
  parseJSON = A.genericParseJSON relayOptions

instance A.ToJSON TokenPair where
  toJSON = A.genericToJSON relayOptions


data WebsocketReq =
  WebsocketReq { websocketreqRequestId :: Integer
               , websocketreqBaseTokenAddress :: T.Text
               , websocketreqQuoteTokenAddress :: T.Text
               , websocketreqSnapshot :: Bool
               , websocketreqSnapshotLimit :: Integer
               }
  deriving (Eq, Show, Generic)

instance A.FromJSON WebsocketReq where
  parseJSON = A.genericParseJSON relayOptions

instance A.ToJSON WebsocketReq where
  toJSON = A.genericToJSON relayOptions


-- JSON Options

relayOptions :: A.Options
relayOptions = aesonPrefix camelCase
