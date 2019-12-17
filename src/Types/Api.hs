{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Api where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Options.Generic
  ( Generic,
  )
import Types (Year, encodingOptions)
import qualified Types.Metric as MetricTypes
import qualified Types.Scenario as ScenarioTypes

data ComparisonListData
  = ComparisonListData {metricData :: HashMap.HashMap Integer MetricTypes.MetricData, scenario :: ScenarioTypes.Scenario, year :: Year}
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON ComparisonListData where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions
