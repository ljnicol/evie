module Init where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Maybe as Maybe
import qualified Options.Generic as OptionsGeneric
import qualified System.Exit as SystemExit
import qualified Types.Config as Config

getConfig :: Config.CommandLine OptionsGeneric.Unwrapped -> IO Config.Config
getConfig (Config.CommandLine cf) = do
  let configFile = Maybe.fromMaybe ("config.json" :: FilePath) $ cf
  configBs <- ByteStringLazyChar8.readFile configFile
  case Aeson.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> "config.json" <> "\nError: " <> e
      SystemExit.exitWith (SystemExit.ExitFailure 2)
    Right config -> pure (addDefaults config)

addDefaults :: Config.InputConfig -> Config.Config
addDefaults Config.InputConfig {..} =
  Config.Config
    _inputConfigApplicationDomain
    _inputConfigApplicationPort
    _inputConfigDB
    _inputConfigTemplates
    _inputConfigStaticDirectory
    _inputConfigSpatialDirectory
