{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Template.Map where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Options.Generic
  ( Generic,
  )
import qualified Text.Ginger.GVal as Ginger
import Types (Year, encodingOptions)
import Types.Metric (Metric, SpatialValues)
import Types.Scenario (Scenario)

data MapTemplateData
  = MapTemplateData {spatial :: HashMap.HashMap Year SpatialValues, metric :: Metric, scenario :: Scenario, host :: String}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON MapTemplateData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance Ginger.ToGVal m MapTemplateData where
  toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td

-- data SpatialValue
--   = SpatialValue
--       { id :: Integer,
--         value :: Double
--       }
--   deriving (Eq, Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

-- instance SQLiteSimple.FromField [SpatialValue] where
--   fromField = fromJSONField

-- instance PGSimple.FromField [SpatialValue] where
--   fromField = PGSimple.fromJSONField

data ComparisonMapTemplateData
  = ComparisonMapTemplateData {scenarioMapData :: [MapTemplateData]}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON ComparisonMapTemplateData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

instance Ginger.ToGVal m ComparisonMapTemplateData where
  toGVal td = Ginger.rawJSONToGVal $ Aeson.toJSON td
