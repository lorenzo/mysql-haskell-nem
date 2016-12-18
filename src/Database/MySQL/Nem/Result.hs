{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

-- |
-- Module:      Database.MySQL.Simple.Result
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  José Lorenzo Rodríguez
-- Stability:   experimental
-- Portability: portable
--
-- The 'Result' typeclass, for converting a single value in a row
-- returned by a SQL query into a more useful Haskell representation.
--
-- A Haskell numeric type is considered to be compatible with all
-- MySQL numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the MySQL 'Long' type
-- because it can represent a 'Long' exactly. On the other hand, since
-- a 'Double' might lose precision if representing a 'LongLong', the
-- two are /not/ considered compatible.
module Database.MySQL.Nem.Result
  ( ResultError(..)
  , Result(..)
  ) where

import Control.Exception (Exception, throw)
import Data.Int
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)
import Data.Typeable (Typeable)
import Database.MySQL.Base (MySQLValue(..), ColumnDef(..))
import qualified Data.ByteString as ByteString (ByteString, unpack)
import qualified Data.Text as Text (Text, unpack)

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError
  = Incompatible { errColumnName :: String
                 , errHaskellType :: String
                 , errMessage :: String}
  | ConversionFailed { errColumnName :: String
                     , errHaskellType :: String
                     , errMessage :: String}
  deriving (Eq, Show, Typeable)

instance Exception ResultError

-- | A type that may be converted from a SQL type.
class Result a  where
  convert :: ColumnDef -> MySQLValue -> a -- Convert a MySQLValue to another value

instance (Result a) =>
         Result (Maybe a) where
  convert def val =
    case val of
      MySQLNull -> Nothing
      _ -> Just $ convert def val

instance Result Int where
  convert = intConvert "Int"

instance Result Int8 where
  convert def val =
    case val of
      MySQLInt8U i -> fromIntegral i
      MySQLInt8 i -> fromIntegral i
      _ -> throw $ conversionFailed "Int8" val def

instance Result Int16 where
  convert def val =
    case val of
      MySQLInt8U i -> fromIntegral i
      MySQLInt16U i -> fromIntegral i
      MySQLInt8 i -> fromIntegral i
      MySQLInt16 i -> fromIntegral i
      _ -> throw $ conversionFailed "Int16" val def

instance Result Int32 where
  convert def val =
    case val of
      MySQLInt8U i -> fromIntegral i
      MySQLInt16U i -> fromIntegral i
      MySQLInt32U i -> fromIntegral i
      MySQLInt8 i -> fromIntegral i
      MySQLInt16 i -> fromIntegral i
      MySQLInt32 i -> fromIntegral i
      _ -> throw $ conversionFailed "Int32" val def

instance Result Int64 where
  convert = intConvert "Int64"

instance Result Float where
  convert def val =
    case val of
      MySQLFloat f -> f
      _ -> throw $ conversionFailed "Float" val def

instance Result Double where
  convert def val =
    case val of
      MySQLDouble d -> d
      _ -> throw $ conversionFailed "Double" val def

instance Result Text.Text where
  convert def val =
    case val of
      MySQLText t -> t
      _ -> throw $ conversionFailed "Text" val def

instance Result String where
  convert def val =
    case val of
      MySQLText t -> Text.unpack t
      _ -> throw $ conversionFailed "String" val def

instance Result ByteString.ByteString where
  convert def val =
    case val of
      MySQLBytes t -> t
      _ -> throw $ conversionFailed "ByteString" val def

instance Result Day where
  convert def val =
    case val of
      MySQLDate d -> d
      _ -> throw $ conversionFailed "Day" val def

instance Result LocalTime where
  convert def val =
    case val of
      MySQLDateTime d -> d
      MySQLTimeStamp d -> d
      _ -> throw $ conversionFailed "LocalTime" val def

instance Result Scientific where
  convert def val =
    case val of
      MySQLDecimal d -> d
      MySQLFloat f -> fromFloatDigits f
      MySQLDouble f -> fromFloatDigits f
      MySQLInt8U i -> fromIntegral i
      MySQLInt16U i -> fromIntegral i
      MySQLInt32U i -> fromIntegral i
      MySQLInt64U i -> fromIntegral i
      MySQLInt8 i -> fromIntegral i
      MySQLInt16 i -> fromIntegral i
      MySQLInt32 i -> fromIntegral i
      MySQLInt64 i -> fromIntegral i
      _ -> throw $ conversionFailed "Scientific" val def

intConvert
  :: Num a
  => String -> ColumnDef -> MySQLValue -> a
intConvert t def val =
  case val of
    MySQLInt8U i -> fromIntegral i
    MySQLInt16U i -> fromIntegral i
    MySQLInt32U i -> fromIntegral i
    MySQLInt64U i -> fromIntegral i
    MySQLInt8 i -> fromIntegral i
    MySQLInt16 i -> fromIntegral i
    MySQLInt32 i -> fromIntegral i
    MySQLInt64 i -> fromIntegral i
    _ -> throw $ conversionFailed t val def

conversionFailed t v def =
  Incompatible
    (show . ByteString.unpack . columnName $ def)
    t
    ("Could not convert: " ++ show v)
