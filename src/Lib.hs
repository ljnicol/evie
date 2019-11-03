
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric             #-}

module Lib
  ( startApp
  )
where

import qualified Data.Aeson                    as Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import qualified Data.Text as Text
import           Control.Monad.Trans            ( liftIO )
import           Servant
import qualified Data.Proxy as Proxy
import           Options.Generic                ( Generic
                                                , ParseRecord
                                                )
import qualified Web.Browser as Browser
import qualified Data.Pool                     as Pool
import qualified Database.PostgreSQL.Simple    as PGSimple
import qualified Data.ByteString               as BS

import qualified Config
import qualified Types.Config as Config

data DbResult = DbResult {
  result1 :: Text.Text
  , result2 :: Text.Text
} deriving (Eq, Generic, Aeson.ToJSON, PGSimple.FromRow)

server
  :: Config.Config -> Pool.Pool PGSimple.Connection -> Server API
server config conns = testDB conns :<|> Servant.serveDirectoryFileServer "static"


testDB
  :: Pool.Pool PGSimple.Connection
  -> Handler [DbResult]
testDB conns = liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query_
    conn
    "SELECT result1, result2 from results"

cookieApi :: Proxy.Proxy API
cookieApi = Proxy.Proxy
  
type API = Unprotected

type Unprotected = "test" :> Get '[JSON] [ DbResult ] :<|> Raw

debug :: Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (requestHeaders req)
  app req resp

startApp :: Config.Config ->  IO ()
startApp config@Config.Config {..} = do
  let connStr = PGSimple.defaultConnectInfo
                  { PGSimple.connectHost     = Config._host _configPG
                  , PGSimple.connectDatabase = Config._database _configPG
                  , PGSimple.connectUser     = Config._user _configPG
                  , PGSimple.connectPassword = Config._password _configPG
                  }
  conns  <- initConnectionPool (PGSimple.postgreSQLConnectionString connStr)
  b <- Browser.openBrowser "http://localhost:7249/test"
  if b then
    run 7249 $ debug $ serve cookieApi (server config conns)
  else
    print "Failed to start browser"


initConnectionPool :: BS.ByteString -> IO (Pool.Pool PGSimple.Connection)
initConnectionPool connStr =
  Pool.createPool (PGSimple.connectPostgreSQL connStr) PGSimple.close 2 -- stripes
                                                                        60 -- unused connections are kept open for a minute
                                                                            10 -- max. 10 connections open per stripe
    