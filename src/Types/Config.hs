module Types.Config where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Options.Generic
  ( Generic,
  )

data InputConfig
  = InputConfig
      { _inputConfigApplicationDomain :: Text.Text,
        _inputConfigApplicationPort :: Int,
        _inputConfigPG :: PGConnectionConfig
      }
  deriving (Show, Generic)

instance Aeson.FromJSON InputConfig where
  parseJSON = Aeson.withObject "Config" $ \o ->
    InputConfig
      <$> o Aeson..: "application_domain"
      <*> o Aeson..: "application_port"
      <*> o Aeson..: "postgres_config"

data Config
  = Config
      { _configApplicationDomain :: Text.Text,
        _configApplicationPort :: Int,
        _configDirectory :: FilePath,
        _configPG :: PGConnectionConfig
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
  parseJSON = Aeson.withObject "Config" $ \o ->
    PostgresConnectionConfig
      <$> o Aeson..: "host"
      <*> o Aeson..: "database"
      <*> o Aeson..: "user"
      <*> o Aeson..: "password"
