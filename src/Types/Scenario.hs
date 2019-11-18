{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Scenario where

import qualified Data.Aeson as Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Typeable (Typeable)
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Database.SQLite.Simple.FromField as SQLiteSimple
import qualified Database.SQLite.Simple.Internal as SQSimple
import Options.Generic
  ( Generic,
  )
import qualified Text.Ginger.GVal as Ginger

encodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

data Scenario
  = Scenario
      { scenarioId :: Integer,
        scenarioName :: Text.Text,
        scenarioDescription :: Text.Text,
        scenarioAssumptions :: Text.Text,
        scenarioSpatialTable :: Text.Text,
        scenarioYears :: [Text.Text]
      }
  deriving (Eq, Generic, Show)

instance SQLiteSimple.FromField [Text.Text] where
  fromField = fromJSONField

fromJSONField :: (Aeson.FromJSON a, Typeable a) => SQLiteSimple.FieldParser a
fromJSONField f@(SQSimple.Field (SQLiteSimple.SQLText blb) _) = do
  case (Aeson.eitherDecode . toLazyByteString . encodeUtf8Builder) blb of
    Left e ->
      SQLiteSimple.returnError SQLiteSimple.ConversionFailed f $
        "JSON decoding error: " ++ e
    Right x -> pure x
fromJSONField f =
  SQLiteSimple.returnError SQLiteSimple.ConversionFailed f $
    "expecting SQLBlob column type"

instance Aeson.ToJSON Scenario where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance SQLiteSimple.FromRow Scenario where
  fromRow =
    Scenario
      <$> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field

-- instance SQLiteSimple.FromField [Text.Text] where
--   fromField = SQLiteSimple.fromJSONField

-- instance SQLiteSimple.FromField [Integer] where
--   fromField = SQLiteSimple.fromJSONField

instance Ginger.ToGVal m Scenario where
  toGVal s = Ginger.rawJSONToGVal $ Aeson.toJSON s

data MetricData
  = MetricData
      { metricDataId :: Integer,
        metricScenarioId :: Integer,
        metricId :: Integer,
        metricName :: Text.Text,
        metricDescription :: Text.Text,
        metricYear :: Text.Text,
        metricValue :: Double,
        metricSpatialTableColumn :: Text.Text
      }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON MetricData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance SQLiteSimple.FromRow MetricData where
  fromRow =
    MetricData
      <$> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field

instance Ginger.ToGVal m MetricData where
  toGVal md = Ginger.rawJSONToGVal $ Aeson.toJSON md

data TemplateData
  = TemplateData {metricData :: HashMap.HashMap Integer MetricData, scenario :: Scenario}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON TemplateData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance Ginger.ToGVal m TemplateData where
  toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td

metricListToHashMap :: [MetricData] -> HashMap.HashMap Integer MetricData
metricListToHashMap ms =
  let toPairs :: MetricData -> (Integer, MetricData)
      toPairs m = (metricId m, m)
   in -- combine ::
      HashMap.fromList (map toPairs ms)
