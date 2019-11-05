{-# LANGUAGE DataKinds #-}
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
        scenarioYears :: [Text.Text]
      }
  deriving (Eq, Generic)

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

instance PGSimple.FromField [Text.Text] where
  fromField = PGSimple.fromJSONField

data ScenarioDetail
  = ScenarioDetail
      { scenarioDetailId :: Integer,
        scenarioDetailYear :: Text.Text,
        metricData :: [Metric]
      }
  deriving (Eq, Generic)

instance Aeson.ToJSON ScenarioDetail where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions

data Metric
  = Metric
      { metricId :: Integer,
        metricName :: Text.Text,
        metricDescription :: Text.Text,
        metricValue :: Double,
        metricSpatialTable :: Text.Text
      }
  deriving (Eq, Generic)

instance Aeson.ToJSON Metric where

  toJSON = Aeson.genericToJSON encodingOptions

  toEncoding = Aeson.genericToEncoding encodingOptions
