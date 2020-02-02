module Lib
  ( startApp,
  )
where

import qualified Controller
import qualified Data.ByteString as ByteString
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.Mbtiles as Mbtiles
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai (run)
import qualified Routes
import qualified Servant
import qualified Types.Config as Config
import qualified Types.DB as DB
import qualified Web.Browser as Browser

debug :: Wai.Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (Wai.requestHeaders req)
  app req resp

startApp :: Config.Config -> IO ()
startApp config@Config.Config {..} = do
  conns <- initDBConnectionPool _configDB
  mbTilesConnsOrError <- Mbtiles.getMbtilesPool (_configSpatialDirectory ++ "/seq_old.mbtiles")
  case mbTilesConnsOrError of
    Left e ->
      print e
    Right spatialConns -> do
      -- conns <- initPostgreSQLConnectionPool dbConnection
      b <- Browser.openBrowser $ Text.unpack _configApplicationDomain ++ ":" ++ (show _configApplicationPort)
      if b
        then Wai.run _configApplicationPort $ debug $ Servant.serve Routes.api (Controller.server config spatialConns $ conns)
        else print "Failed to start browser"

initDBConnectionPool :: Config.DBConfig -> IO (DB.DatabaseEngine a)
initDBConnectionPool dbConfig =
  case dbConfig of
    Config.SQLiteConfig f ->
      fmap DB.SQLite3 $ initSQLiteConnectionPool f
    Config.PGConfig pgConfig ->
      fmap DB.PostgreSQL $ initPostgreSQLConnectionPool dbConnection
      where
        dbConnection =
          PGSimple.postgreSQLConnectionString $
            PGSimple.defaultConnectInfo
              { PGSimple.connectHost = Config._host pgConfig,
                PGSimple.connectDatabase = Config._database pgConfig,
                PGSimple.connectUser = Config._user pgConfig,
                PGSimple.connectPassword = Config._password pgConfig
              }

initSQLiteConnectionPool :: String -> IO (Pool.Pool SQLiteSimple.Connection)
initSQLiteConnectionPool dbFile =
  Pool.createPool
    (SQLiteSimple.open dbFile)
    SQLiteSimple.close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe

initPostgreSQLConnectionPool :: ByteString.ByteString -> IO (Pool.Pool PGSimple.Connection)
initPostgreSQLConnectionPool connStr =
  Pool.createPool
    (PGSimple.connectPostgreSQL connStr)
    PGSimple.close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe
