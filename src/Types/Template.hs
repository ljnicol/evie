{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Template where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Database.PostgreSQL.Simple.FromField as PGSimple
import qualified Database.SQLite.Simple.FromField as SQLiteSimple
import Options.Generic
  ( Generic,
  )
import qualified Text.Ginger.GVal as Ginger
import Types (Year, encodingOptions)
import Types.DB (fromJSONField)
import Types.Metric (MetricData)
import Types.Scenario (Scenario)

-- data TemplateDataYear
--   = TemplateDataYear {metricData :: HashMap.HashMap Integer (HashMap.HashMap Year MetricData), scenario :: Scenario, year :: Year, host :: String}
--   deriving (Eq, Generic, Show)

-- instance Aeson.ToJSON TemplateDataYear where

--   toJSON = Aeson.genericToJSON encodingOptions

--   toEncoding = Aeson.genericToEncoding encodingOptions

-- instance Ginger.ToGVal m TemplateDataYear where
--   toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td

data TemplateData
  = TemplateData {metricData :: HashMap.HashMap Integer MetricData, scenario :: Scenario, year :: Year, host :: String}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON TemplateData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance Ginger.ToGVal m TemplateData where
  toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td

data ComparisonTemplateData
  = ComparisonTemplateData {scenarioData :: [TemplateData]}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON ComparisonTemplateData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance Ginger.ToGVal m ComparisonTemplateData where
  toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td

data MapTemplateData
  = MapTemplateData {spatialValues :: [SpatialValue]}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON MapTemplateData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance Ginger.ToGVal m MapTemplateData where
  toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td

data SpatialValue
  = SpatialValue
      { id :: Integer,
        value :: Double
      }
  deriving (Eq, Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

instance SQLiteSimple.FromField [SpatialValue] where
  fromField = fromJSONField

instance PGSimple.FromField [SpatialValue] where
  fromField = PGSimple.fromJSONField
