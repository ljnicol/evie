module Init where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Maybe as Maybe
import qualified System.Exit as SystemExit
import qualified Types.Config as Config

getConfig :: Maybe FilePath -> IO Config.Config
getConfig cmdLineAppDirectory = do
  let appDirectory = Maybe.fromMaybe "." cmdLineAppDirectory
  configBs <- ByteStringLazyChar8.readFile (appDirectory ++ "/" ++ "config.json")
  case Aeson.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> "config.json" <> "\nError: " <> e
      SystemExit.exitWith (SystemExit.ExitFailure 2)
    Right config -> pure (addDefaults config appDirectory)

addDefaults :: Config.InputConfig -> FilePath -> Config.Config
addDefaults Config.InputConfig {..} appDirectory =
  Config.Config
    _inputConfigApplicationDomain
    _inputConfigApplicationPort
    appDirectory
    _inputConfigPG
