{-# LANGUAGE OverloadedStrings #-}

module Relay.DB where

import Relay.Types
import qualified Database.Selda.Generic    as SG

radarOrders :: SG.GenTable ExchangeOrder
radarOrders = SG.genTable ("radar_order") []

dexOrders :: SG.GenTable ExchangeOrder
dexOrders = SG.genTable ("dex_order") []

tokens :: SG.GenTable Token
tokens = SG.genTable "token" []
