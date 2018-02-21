{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Relay.Types
  ( HexString
  , makeHexString
  , Address
  , makeAddress
  , Quantity(..)
  , Timestamp(..)
  , ECSignature(..)
  , ExchangeOrder(..)
  , OrderBook(..)
  , Token(..)
  , TokenPair(..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Maybe (fromJust)
import Data.ByteString.Short as  SBS
import Data.ByteString.Base16 as BS16
import Crypto.Secp256k1 as Crypto
import qualified Data.Text.Encoding as T
import Data.Monoid
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TB (hexadecimal)
import qualified Data.Text.Read as R (hexadecimal, decimal)
import GHC.Generics (Generic)
import Control.Error (hush, fmapL)
import Text.Parsec as P
import Relay.Utils (Example(..))


--------------------------------------------------------------------------------
-- * Basic Types
--------------------------------------------------------------------------------

-- | HexString
newtype HexString =
  HexString { unHexString :: T.Text }
  deriving (Eq, Show, Generic)

parseHexString' :: T.Text -> Either String HexString
parseHexString' = fmapL show . fmap (HexString . T.pack) . P.parse (P.many1 P.hexDigit) "" . strip0x

makeHexString :: T.Text -> Maybe HexString
makeHexString = hush . parseHexString'

instance A.FromJSON HexString where
  parseJSON (A.String s) = either fail return . parseHexString' $ s
  parseJSON a = A.typeMismatch "HexString" a

instance A.ToJSON HexString where
  toJSON (HexString hx) = A.toJSON $ "0x" <> hx

instance Example HexString where
  example _ = HexString "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7"

-- | Address
newtype Address =
  Address { unAddress :: HexString }
  deriving (Eq, Show, Generic)

makeAddress :: T.Text -> Maybe Address
makeAddress a = case parseHexString' a of
  Left _ -> Nothing
  Right hx -> if (T.length . unHexString $ hx) == 40 then Just (Address hx) else Nothing

instance A.FromJSON Address where
  parseJSON (A.String s) =
    case parseHexString' s of
      Right hx ->
        if (T.length . unHexString $ hx) == 40
          then return . Address $ hx
          else fail "Address must be 20 bytes long."
      Left err -> fail err
  parseJSON a = A.typeMismatch "Address" a

instance A.ToJSON Address where
  toJSON = A.toJSON . unAddress

instance Example Address where
  example _ = Address . HexString $ "123f681646d4a755815f9cb19e1acc8565a0c2ac"

-- | Quantity
newtype Quantity =
  Quantity { unQuantity :: Integer }
  deriving (Eq, Show, Generic)

instance A.FromJSON Quantity where
  parseJSON (A.String s)=
      case R.hexadecimal . strip0x $ s of
        Left err -> fail err
        Right (hx, "") -> return . Quantity $ hx
        _ -> fail $ "Failed to parse as Quantity : " <> show s
  parseJSON a = A.typeMismatch "Quantity" a

instance A.ToJSON Quantity where
  toJSON =  A.toJSON . mappend "0x" . toLazyText . TB.hexadecimal . unQuantity

newtype Timestamp =
  Timestamp { unTimestamp :: Integer }
  deriving (Eq, Show, Generic)

instance A.FromJSON Timestamp where
  parseJSON (A.String s) =
    case R.decimal s of
      Right (ts, "") -> return . Timestamp $ ts
      Right _ -> fail $ "Failed to parse as Timestamp : " <> show s
      Left err -> fail err
  parseJSON a = A.typeMismatch "Timestamp" a

instance A.ToJSON Timestamp where
  toJSON = A.toJSON . show . unTimestamp

newtype Salt =
  Salt { unSalt :: Integer }
  deriving (Eq, Show, Generic)

instance A.FromJSON Salt where
  parseJSON (A.String s) =
    case R.decimal s of
      Right (ts, "") -> return . Salt $ ts
      Right _ -> fail $ "Failed to parse as Salt : " <> show s
      Left err -> fail err
  parseJSON a = A.typeMismatch "Salt" a

instance A.ToJSON Salt where
  toJSON = A.toJSON . show . unSalt

newtype ECSignature =
  ECSignature { unECSignature :: Crypto.CompactRecSig }
  deriving (Eq, Show, Generic)

instance A.ToJSON ECSignature where
  toJSON (ECSignature (Crypto.CompactRecSig r s v)) =
    let getRS = T.decodeUtf8 . BS16.encode . SBS.fromShort
        getV = toInteger
    in A.object [ "r" A..= getRS r
                , "s" A..= getRS s
                , "v" A..= getV v
                ]

instance A.FromJSON ECSignature where
  parseJSON (A.Object o) = do
    let getRS = SBS.toShort . fst . BS16.decode . T.encodeUtf8
        getV = read
    r <- getRS <$> o A..: "r"
    s <- getRS <$> o A..: "s"
    v <- getV <$> o A..: "v"
    return . ECSignature $ Crypto.CompactRecSig r s v
  parseJSON a = A.typeMismatch "ECSignature" a

instance Example ECSignature where
  example _ = fromJust . A.decode . A.encode $
    A.object [ "r" A..= ("61a3ed31b43c8780e905a260a35faefcc527be7516aa11c0256729b5b351bc33" :: T.Text)
             , "s" A..= ("40349190569279751135161d22529dc25add4f6069af05be04cacbda2ace2254" :: T.Text)
             , "v" A..= ("27" :: T.Text)
             ]

strip0x :: T.Text -> T.Text
strip0x t = case T.take 2 t of
  "0x" -> T.drop 2 t
  _ -> t

--------------------------------------------------------------------------------
-- * Order Types
--------------------------------------------------------------------------------

-- | Exchange Order

data ExchangeOrder =
  ExchangeOrder { exchangeorderExchangeContractAddress :: Address
                , exchangeorderMaker :: Address
                , exchangeorderTaker :: Address
                , exchangeorderMakerTokenAddress :: Address
                , exchangeorderTakerTokenAddress :: Address
                , exchangeorderMakerTokenAmount :: Quantity
                , exchangeorderTakerTokenAmount :: Quantity
                , exchangeorderMakerFee :: Quantity
                , exchangeorderTakerFee :: Quantity
                , exchangeorderFeeRecipient :: Address
                , exchangeorderExpirationUnixTimestampSec :: Timestamp
                , exchangeorderSalt :: Salt
                , exchangeorderEcSignature :: ECSignature
                }
  deriving (Eq, Show, Generic)

instance A.FromJSON ExchangeOrder where
  parseJSON = A.genericParseJSON relayOptions

instance A.ToJSON ExchangeOrder where
  toJSON = A.genericToJSON relayOptions

instance Example ExchangeOrder where
  example _ =
    ExchangeOrder { exchangeorderExchangeContractAddress = Address . HexString $ "12459c951127e0c374ff9105dda097662a027093"
                  , exchangeorderMaker = Address . HexString $ "9e56625509c2f60af937f23b7b532600390e8c8b"
                  , exchangeorderTaker = Address . HexString $ "a2b31dacf30a9c50ca473337c01d8a201ae33e32"
                  , exchangeorderMakerTokenAddress = Address . HexString $ "323b5d4c32345ced77393b3530b1eed0f346429d"
                  , exchangeorderTakerTokenAddress = Address . HexString $ "ef7fff64389b814a946f3e92105513705ca6b990"
                  , exchangeorderMakerTokenAmount = Quantity 10000000000000000
                  , exchangeorderTakerTokenAmount = Quantity 20000000000000000
                  , exchangeorderMakerFee = Quantity 100000000000000
                  , exchangeorderTakerFee = Quantity 200000000000000
                  , exchangeorderFeeRecipient = Address . HexString $ "b046140686d052fff581f63f8136cce132e857da"
                  , exchangeorderExpirationUnixTimestampSec = Timestamp 42
                  , exchangeorderSalt = Salt 54515451557974875123697849345751275676157243756715784155226239582178
                  , exchangeorderEcSignature = example (Proxy @ECSignature)
                  }

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
  Token { tokenAddress :: Address
        , tokenMinAmount :: Quantity
        , tokenMaxAmount :: Quantity
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

-- JSON Options

relayOptions :: A.Options
relayOptions = aesonPrefix camelCase

