{-# LANGUAGE DataKinds #-}
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
        _inputConfigApplicationPort :: Int
      }
  deriving (Show, Generic)

instance Aeson.FromJSON InputConfig where
  parseJSON = Aeson.withObject "Config" $ \o ->
    InputConfig
      <$> o Aeson..: "application_domain"
      <*> o Aeson..: "application_port"

data Config
  = Config
      { _configApplicationDomain :: Text.Text,
        _configApplicationPort :: Int,
        _configDirectory :: FilePath,
        _configDataFile :: FilePath
      }

data CommandLine w
  = CommandLine
      { appDirectory :: w ::: Maybe FilePath <?> "Default: \".\"",
        dataFile :: w ::: Maybe FilePath <?> "Default: \"data.sqlite\" in the app directory"
      }
  deriving (Generic)

instance ParseRecord (CommandLine Wrapped)

deriving instance Show (CommandLine Unwrapped)
