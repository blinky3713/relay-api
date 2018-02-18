{-# LANGUAGE OverloadedStrings #-}

module Relay.APISpec where

import Data.Either
import Relay
import Test.Hspec
import System.IO.Unsafe (unsafePerformIO)


spec :: Spec
spec = describe "Contacting the Relay API." $
  it "can contact the api with no empty returns" $ do
   -- etokenPairs <- runClientM (getTokenPairs []) relayEnv
   -- etokenPairs `shouldSatisfy` isRight
   -- let Right tps = etokenPairs
   -- take 5 tps `shouldNotBe` []

    orders <- runClientM (getExchangeOrders (Just 1) (Just 1) [] [] [] [] [] [] [] []) relayEnv
    orders `shouldSatisfy` isRight
    let Right os = orders
    os `shouldNotBe` []

    orders <- runClientM (getOrderBook (Just 1) (Just 1) ["0x6810e776880c02933d47db1b9fc05908e5386b96"] ["0x6810e776880c02933d47db1b9fc05908e5386b96"]) relayEnv
    orders `shouldSatisfy` isRight

    order <- runClientM (getOrderByHash "0xe91e990bab4c9c6bd60ff3673222390e3da8b7bd9a50eab98a8cb20723b24ee1") relayEnv
    orders `shouldSatisfy` isRight


relayEnv :: ClientEnv
relayEnv = unsafePerformIO $ mkRelayClientEnv "api.radarrelay.com" "/0x/v0" 80
{-# NOINLINE relayEnv #-}

