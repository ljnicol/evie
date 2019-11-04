{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified System.Exit as SystemExit
import qualified Types.Config as Config

getConfig :: FilePath -> IO Config.Config
getConfig cfgFilePath = do
  configBs <- ByteStringLazyChar8.readFile cfgFilePath
  case Aeson.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> cfgFilePath <> "\nError: " <> e
      SystemExit.exitWith (SystemExit.ExitFailure 2)
    Right config -> pure (addDefaults config)

addDefaults :: Config.InputConfig -> Config.Config
addDefaults Config.InputConfig {..} =
  Config.Config
    _inputConfigApplicationDomain
    _inputConfigPG
