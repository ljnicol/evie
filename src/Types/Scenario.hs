{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Types.Scenario where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.FromField as PGSimple
import qualified Database.PostgreSQL.Simple.FromRow as PGSimple
import Options.Generic
import qualified Servant.EDE as EDE

data Scenarios
  = Scenarios
      { scenarios :: [Scenario]
      }
  deriving (Eq, Generic, Aeson.ToJSON)

instance EDE.ToObject Scenarios

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
  deriving (Eq, Generic)

instance EDE.ToObject Scenario

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
  deriving (Eq, Generic)

instance EDE.ToObject MetricData

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
