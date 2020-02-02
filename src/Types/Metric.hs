{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Metric where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.FromField as PGSimple
import qualified Database.PostgreSQL.Simple.FromRow as PGSimple
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Database.SQLite.Simple.FromField as SQLiteSimple
import Options.Generic
  ( Generic,
  )
import qualified Text.Ginger.GVal as Ginger
import Types (Year, encodingOptions)
import qualified Types.DB as DBTypes

data MetricName
  = MetricName
      { id :: Integer,
        name :: Text.Text
      }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON MetricName where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance SQLiteSimple.FromRow MetricName where
  fromRow =
    MetricName
      <$> SQLiteSimple.field
      <*> SQLiteSimple.field

instance PGSimple.FromRow MetricName where
  fromRow =
    MetricName
      <$> PGSimple.field
      <*> PGSimple.field

data Metric
  = Metric
      { metricId :: Integer,
        metricName :: Text.Text,
        metricDescription :: Text.Text,
        metricLowOutcome :: Double,
        metricLowOutcomeText :: Text.Text,
        metricHighOutcome :: Double,
        metricHighOutcomeText :: Text.Text,
        metricBins :: [(Double, Text.Text)],
        metricUnit :: Text.Text
      }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON Metric where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance SQLiteSimple.FromRow Metric where
  fromRow =
    Metric
      <$> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field

instance PGSimple.FromRow Metric where
  fromRow =
    Metric
      <$> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field

instance Ginger.ToGVal m Metric where
  toGVal md = Ginger.rawJSONToGVal $ Aeson.toJSON md

data SpatialData
  = SpatialData
      { year :: Text.Text,
        spatialValues :: SpatialValues
      }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON SpatialData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance SQLiteSimple.FromRow SpatialData where
  fromRow =
    SpatialData
      <$> SQLiteSimple.field
      <*> SQLiteSimple.field

instance PGSimple.FromRow SpatialData where
  fromRow =
    SpatialData
      <$> PGSimple.field
      <*> PGSimple.field

instance Ginger.ToGVal m SpatialData where
  toGVal md = Ginger.rawJSONToGVal $ Aeson.toJSON md

instance SQLiteSimple.FromField [(Double, Text.Text)] where
  fromField = DBTypes.fromJSONField

instance PGSimple.FromField [(Double, Text.Text)] where
  fromField = PGSimple.fromJSONField

spatialMetricsToHashMap :: [SpatialData] -> HashMap.HashMap Year SpatialValues
spatialMetricsToHashMap ms =
  let toPairs :: SpatialData -> (Year, SpatialValues)
      toPairs m = (year m, spatialValues m)
   in -- combine ::
      HashMap.fromList (map toPairs ms)

type SpatialValues = Text.Text
