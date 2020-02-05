{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Types.Config where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Options.Generic

data InputConfig
  = InputConfig
      { _inputConfigApplicationDomain :: Text.Text,
        _inputConfigApplicationPort :: Int,
        _inputConfigDB :: DBConfig,
        _inputConfigTemplates :: FilePath,
        _inputConfigStaticDirectory :: FilePath,
        _inputConfigSpatialDirectory :: FilePath,
        _inputConfigLibDirectory :: FilePath
      }
  deriving (Show, Generic)

instance Aeson.FromJSON InputConfig where
  parseJSON = Aeson.withObject "Config" $ \o ->
    InputConfig
      <$> o Aeson..: "application_domain"
      <*> o Aeson..: "application_port"
      <*> o Aeson..: "db_config"
      <*> o Aeson..: "template_directory"
      <*> o Aeson..: "static_directory"
      <*> o Aeson..: "spatial_directory"
      <*> o Aeson..: "lib_directory"

data DBConfig = SQLiteConfig FilePath | PGConfig PGConnectionConfig deriving (Show, Generic)

instance Aeson.FromJSON DBConfig where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.sumEncoding = Aeson.UntaggedValue}

data Config
  = Config
      { _configApplicationDomain :: Text.Text,
        _configApplicationPort :: Int,
        _configDB :: DBConfig,
        _configTemplates :: FilePath,
        _configStaticDirectory :: FilePath,
        _configSpatialDirectory :: FilePath,
        _configLibDirectory :: FilePath
      }

data PGConnectionConfig
  = PostgresConnectionConfig
      { _host :: String,
        _database :: String,
        _user :: String,
        _password :: String
      }
  deriving (Show, Generic)

instance Aeson.FromJSON PGConnectionConfig where
  parseJSON = Aeson.withObject "PGConnectionConfig" $ \o ->
    PostgresConnectionConfig
      <$> o Aeson..: "host"
      <*> o Aeson..: "database"
      <*> o Aeson..: "user"
      <*> o Aeson..: "password"

data CommandLine w
  = CommandLine
      { configFile :: w ::: Maybe FilePath <?> "Default: \"config.json\""
      }
  deriving (Generic)

instance ParseRecord (CommandLine Wrapped)

deriving instance Show (CommandLine Unwrapped)
