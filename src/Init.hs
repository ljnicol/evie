module Init where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Maybe as Maybe
import qualified Options.Generic as OptionsGeneric
import qualified System.Exit as SystemExit
import qualified Types.Config as Config

getConfig :: Config.CommandLine OptionsGeneric.Unwrapped -> IO Config.Config
getConfig (Config.CommandLine d df) = do
  let appDirectory = Maybe.fromMaybe ("." :: FilePath) $ d
      dataFile = Maybe.fromMaybe (appDirectory ++ "/" ++ "data.sqlite" :: FilePath) $ df
  configBs <- ByteStringLazyChar8.readFile (appDirectory ++ "/" ++ "config.json")
  case Aeson.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> "config.json" <> "\nError: " <> e
      SystemExit.exitWith (SystemExit.ExitFailure 2)
    Right config -> pure (addDefaults config appDirectory dataFile)

addDefaults :: Config.InputConfig -> FilePath -> FilePath -> Config.Config
addDefaults Config.InputConfig {..} appDirectory dataFile =
  Config.Config
    _inputConfigApplicationDomain
    _inputConfigApplicationPort
    appDirectory
    dataFile
    _inputConfigPG
    _inputConfigTemplates
    _inputConfigStaticDirectory
    _inputConfigSpatialDirectory

