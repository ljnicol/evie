{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Types.Config where

import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as Text

import           Options.Generic                ( Generic
                                                , ParseRecord
                                                )

data InputConfig = InputConfig
    {
         _inputConfigApplicationDomain :: Text.Text
        , _inputConfigPG :: PGConnectionConfig
    } deriving (Show, Generic)

instance Aeson.FromJSON InputConfig where
    parseJSON = Aeson.withObject "Config" $ \o -> InputConfig
        <$> o Aeson..: "application_domain"
        <*> o Aeson..: "postgres_config"

data Config = Config
    {
         _configApplicationDomain :: Text.Text
        , _configPG :: PGConnectionConfig
    }

data PGConnectionConfig = PostgresConnectionConfig{
    _host :: String
    , _database :: String
    , _user :: String
    , _password :: String
} deriving (Show, Generic)

instance Aeson.FromJSON PGConnectionConfig where
    parseJSON = Aeson.withObject "Config" $ \o -> PostgresConnectionConfig
        <$> o Aeson..: "host"
        <*> o Aeson..: "database"
        <*> o Aeson..: "user"
        <*> o Aeson..:  "password"
