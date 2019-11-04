{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Lib
  ( startApp,
  )
where

import qualified Config
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Options.Generic
  ( Generic,
    ParseRecord,
  )
import Servant
import qualified Types.Config as Config
import qualified Web.Browser as Browser

data DbResult
  = DbResult
      { result1 :: Text.Text,
        result2 :: Text.Text
      }
  deriving (Eq, Generic, Aeson.ToJSON, PGSimple.FromRow)

server ::
  Config.Config -> Pool.Pool PGSimple.Connection -> Server API
server config conns = testDB conns :<|> Servant.serveDirectoryFileServer "static"

testDB ::
  Pool.Pool PGSimple.Connection ->
  Handler [DbResult]
testDB conns = liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query_
    conn
    "SELECT result1, result2 from results"

cookieApi :: Proxy.Proxy API
cookieApi = Proxy.Proxy

type API = Unprotected

type Unprotected = "test" :> Get '[JSON] [DbResult] :<|> Raw

debug :: Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (requestHeaders req)
  app req resp

startApp :: Config.Config -> IO ()
startApp config@Config.Config {..} = do
  let connStr =
        PGSimple.defaultConnectInfo
          { PGSimple.connectHost = Config._host _configPG,
            PGSimple.connectDatabase = Config._database _configPG,
            PGSimple.connectUser = Config._user _configPG,
            PGSimple.connectPassword = Config._password _configPG
          }
  conns <- initConnectionPool (PGSimple.postgreSQLConnectionString connStr)
  b <- Browser.openBrowser "http://localhost:7249/test"
  if b
    then run 7249 $ debug $ serve cookieApi (server config conns)
    else print "Failed to start browser"

initConnectionPool :: BS.ByteString -> IO (Pool.Pool PGSimple.Connection)
initConnectionPool connStr =
  Pool.createPool
    (PGSimple.connectPostgreSQL connStr)
    PGSimple.close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe
