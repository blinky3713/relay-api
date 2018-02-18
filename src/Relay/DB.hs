{-# LANGUAGE OverloadedStrings #-}

module Relay.DB where

import Relay.Types
import qualified Database.Selda.Generic    as SG

orders :: String -> SG.GenTable ExchangeOrder
orders s = SG.genTable (s <> _"order") []

tokens :: SG.GenTable Token
tokens = SG.genTable "token" []
