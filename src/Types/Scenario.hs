{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Scenario where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.FromField as PGSimple
import qualified Database.PostgreSQL.Simple.FromRow as PGSimple
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
        scenarioYears :: [Integer]
      }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON Scenario where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance PGSimple.FromRow Scenario where
  fromRow =
    Scenario
      <$> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field

instance PGSimple.FromField [Text.Text] where
  fromField = PGSimple.fromJSONField

instance PGSimple.FromField [Integer] where
  fromField = PGSimple.fromJSONField

instance Ginger.ToGVal m Scenario where
  toGVal s = Ginger.rawJSONToGVal $ Aeson.toJSON s

data MetricData
  = MetricData
      { metricDataId :: Integer,
        metricScenarioId :: Integer,
        metricId :: Integer,
        metricName :: Text.Text,
        metricDescription :: Text.Text,
        metricYear :: Integer,
        metricValue :: Double,
        metricSpatialTableColumn :: Text.Text
      }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON MetricData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

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
