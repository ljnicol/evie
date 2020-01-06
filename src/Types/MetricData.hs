{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.MetricData where

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

data MetricData
  = MetricData
      { metricScenarioId :: Integer,
        metricId :: Integer,
        metricName :: Text.Text,
        metricDescription :: Text.Text,
        metricLowOutcome :: Double,
        metricLowOutcomeText :: Text.Text,
        metricHighOutcome :: Double,
        metricHighOutcomeText :: Text.Text,
        metricBins :: [(Double, Text.Text)],
        metricUnit :: Text.Text,
        metricYear :: Text.Text,
        metricValue :: Double,
        metricSpatial :: Text.Text
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
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field
      <*> SQLiteSimple.field

instance PGSimple.FromRow MetricData where
  fromRow =
    MetricData
      <$> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field

instance SQLiteSimple.FromField [(Double, Text.Text)] where
  fromField = DBTypes.fromJSONField

instance PGSimple.FromField [(Double, Text.Text)] where
  fromField = PGSimple.fromJSONField

instance Ginger.ToGVal m MetricData where
  toGVal md = Ginger.rawJSONToGVal $ Aeson.toJSON md

metricListToHashMap :: [MetricData] -> HashMap.HashMap Integer MetricData
metricListToHashMap ms =
  let toPairs :: MetricData -> (Integer, MetricData)
      toPairs m = (metricId m, m)
   in -- combine ::
      HashMap.fromList (map toPairs ms)

metricListToHashMapYear :: [MetricData] -> HashMap.HashMap Integer (HashMap.HashMap Year MetricData)
metricListToHashMapYear ms =
  let toPairs :: MetricData -> (Integer, [(Year, MetricData)])
      toPairs m = (metricId m, [(metricYear m, m)])
   in -- combine ::
      HashMap.map (HashMap.fromList) $ HashMap.fromListWith (++) (map toPairs ms)
