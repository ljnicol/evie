{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Scenario where

import qualified Data.Aeson as Aeson
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
import qualified Types.DB as DBTypes
import Types (Year, encodingOptions)

data Scenario
  = Scenario
      { scenarioId :: Integer,
        scenarioName :: Text.Text,
        scenarioDescription :: Text.Text,
        scenarioAssumptions :: Text.Text,
        scenarioYears :: [Text.Text]
      }
  deriving (Eq, Generic, Show)

instance SQLiteSimple.FromField [Text.Text] where
  fromField = DBTypes.fromJSONField

instance PGSimple.FromField [Text.Text] where
  fromField = PGSimple.fromJSONField

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

instance PGSimple.FromRow Scenario where
  fromRow =
    Scenario
      <$> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field
      <*> PGSimple.field

instance Ginger.ToGVal m Scenario where
  toGVal s = Ginger.rawJSONToGVal $ Aeson.toJSON s

