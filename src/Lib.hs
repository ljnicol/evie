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
import qualified Types.Scenario as ScenarioTypes
import qualified Web.Browser as Browser

server ::
  Config.Config -> Pool.Pool PGSimple.Connection -> Server API
server config@Config.Config {..} conns = scenariosDB conns :<|> metricsDB conns :<|> Servant.serveDirectoryFileServer (_configDirectory ++ "/" ++ "static")

scenariosDB ::
  Pool.Pool PGSimple.Connection ->
  Handler [ScenarioTypes.Scenario]
scenariosDB conns = liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query_
    conn
    "SELECT id, name, description, assumptions, spatial_table, md.years from scenarios join (select json_agg(year) as years, scenario_id from (select distinct year, scenario_id from metric_data order by year) as a group by scenario_id ) as md on scenarios.id = md.scenario_id"

metricsDB ::
  Pool.Pool PGSimple.Connection ->
  Int ->
  Int ->
  Handler [ScenarioTypes.MetricData]
metricsDB conns scenarioId year = liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query
    conn
    "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? and year = ? order by metric_id"
    (scenarioId, year)

cookieApi :: Proxy.Proxy API
cookieApi = Proxy.Proxy

type API = Unprotected

type Unprotected =
  "scenarios"
    :> Get '[JSON] [ScenarioTypes.Scenario]
      :<|> "metrics"
    :> Capture "scenario_id" Int
    :> Capture "year" Int
    :> Get '[JSON] [ScenarioTypes.MetricData] :<|> Raw

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
  b <- Browser.openBrowser $ Text.unpack _configApplicationDomain ++ ":" ++ (show _configApplicationPort)
  if b
    then run _configApplicationPort $ debug $ serve cookieApi (server config conns)
    else print "Failed to start browser"

initConnectionPool :: BS.ByteString -> IO (Pool.Pool PGSimple.Connection)
initConnectionPool connStr =
  Pool.createPool
    (PGSimple.connectPostgreSQL connStr)
    PGSimple.close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe
