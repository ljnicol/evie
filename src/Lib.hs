module Lib
  ( startApp,
  )
where

import qualified Controller
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai (run)
import qualified Routes
import qualified Servant
import qualified Types.Config as Config
import qualified Web.Browser as Browser

debug :: Wai.Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (Wai.requestHeaders req)
  app req resp

startApp :: Config.Config -> IO ()
startApp config@Config.Config {..} = do
  let dbFile = "data/data.sqlite"
  -- SQLiteSimple.defaultConnectInfo
  --   { SQLiteSimple.connectHost = Config._host _configPG,
  --     SQLiteSimple.connectDatabase = Config._database _configPG,
  --     SQLiteSimple.connectUser = Config._user _configPG,
  --     SQLiteSimple.connectPassword = Config._password _configPG
  --   }
  conns <- initConnectionPool dbFile
  b <- Browser.openBrowser $ Text.unpack _configApplicationDomain ++ ":" ++ (show _configApplicationPort) ++ "/app"
  if b
    then Wai.run _configApplicationPort $ debug $ Servant.serve Routes.api (Controller.server config conns)
    else print "Failed to start browser"

initConnectionPool :: String -> IO (Pool.Pool SQLiteSimple.Connection)
initConnectionPool dbFile =
  Pool.createPool
    (SQLiteSimple.open dbFile)
    SQLiteSimple.close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe
