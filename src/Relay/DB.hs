{-# LANGUAGE OverloadedStrings #-}

module Relay.DB where

import Relay.Types
import qualified Database.Selda.Generic    as SG

orders :: SG.GenTable ExchangeOrder
orders = SG.genTable "order" []
