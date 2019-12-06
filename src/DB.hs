{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module DB where

import Control.Monad (mapM)
import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Char as Char
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Read as TextRead
import qualified Database.Mbtiles as Mbtiles
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.SqlQQ as PGQQ
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Database.SQLite.Simple.QQ as SQQQ
import qualified Errors
import qualified Servant
import qualified Types.DB as DBTypes
import qualified Types.Scenario as ScenarioTypes

--

getScenarioDetailDB :: DBTypes.DatabaseEngine a -> Integer -> ScenarioTypes.Year -> String -> Servant.Handler ScenarioTypes.TemplateData
getScenarioDetailDB dbEngine scenarioId year host = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ metricsDBForYear dbEngine scenarioId year
  scenario <- scenarioDB dbEngine scenarioId
  return $ ScenarioTypes.TemplateData metrics scenario year host

-- getScenarioMapDB :: DBTypes.DatabaseEngine a -> Integer -> Integer -> Servant.Handler ScenarioTypes.TemplateData
-- getScenarioMapDB dbEngine scenarioId metricId = do
--   spatialData <- spatialDataDB dbEngine scenarioId metricId
--   scenario <- scenarioDB dbEngine scenarioId
--   return $ ScenarioTypes.MapTemplateData spatialData scenario

getScenarioComparisonListDB :: DBTypes.DatabaseEngine a -> [Integer] -> ScenarioTypes.Year -> Servant.Handler [ScenarioTypes.TemplateData]
getScenarioComparisonListDB dbEngine scenarioIds year =
  let toTD :: ScenarioTypes.Year -> ScenarioTypes.Scenario -> Servant.Handler ScenarioTypes.TemplateData
      toTD year scenario = do
        metrics <- fmap ScenarioTypes.metricListToHashMap $ metricsDBForYear dbEngine (ScenarioTypes.scenarioId scenario) year
        pure $ ScenarioTypes.TemplateData metrics scenario year ""
   in do
        -- TODO: What happens if there is no data for a scenario in the requested year.
        scenarios <- scenariosSelectDB dbEngine scenarioIds
        mapM (toTD year) scenarios

scenarioDB ::
  DBTypes.DatabaseEngine a ->
  Integer ->
  Servant.Handler ScenarioTypes.Scenario
scenarioDB dbEngine scenarioId = do
  res <-
    MonadTrans.liftIO $ action
  case res of
    x : xs ->
      return x
    _ ->
      Servant.throwError Servant.err401 {Servant.errBody = Errors.errorString "401" "No results found" "Try a different scenario or contact your support team."}
  where
    action =
      case dbEngine of
        DBTypes.SQLite3 conns -> Pool.withResource conns $ \conn ->
          SQLiteSimple.query
            conn
            sqQuery
            (SQLiteSimple.Only $ scenarioId)
        DBTypes.PostgreSQL conns -> Pool.withResource conns $ \conn ->
          PGSimple.query
            conn
            pgQuery
            (PGSimple.Only $ scenarioId)
    sqQuery =
      [SQQQ.sql| 
        SELECT id, name, description, assumptions, md.years 
        from scenarios 
        join (
          select json_group_array(year) as years, scenario_id 
          from (select distinct year, scenario_id from metric_data order by year) as a group by scenario_id 
          ) as md 
        on scenarios.id = md.scenario_id 
        where scenario_id = ?
        |]
    pgQuery =
      [PGQQ.sql| 
        SELECT id, name, description, assumptions, md.years 
        from scenarios 
        join (
            select json_agg(year) as years, scenario_id 
            from (select distinct year, scenario_id from metric_data order by year) 
            as a group by scenario_id 
            ) as md 
        on scenarios.id = md.scenario_id 
        where scenario_id = ?
        |]

scenariosSelectDB ::
  DBTypes.DatabaseEngine a ->
  [Integer] ->
  Servant.Handler [ScenarioTypes.Scenario]
scenariosSelectDB dbEngine scenarioIds =
  mapM
    ( scenarioDB dbEngine
    )
    scenarioIds

scenariosDB ::
  DBTypes.DatabaseEngine a ->
  Servant.Handler [ScenarioTypes.Scenario]
scenariosDB dbEngine =
  MonadTrans.liftIO $ action
  where
    action =
      case dbEngine of
        DBTypes.SQLite3 conns -> Pool.withResource conns $ \conn ->
          SQLiteSimple.query_
            conn
            sqQuery
        DBTypes.PostgreSQL conns -> Pool.withResource conns $ \conn ->
          PGSimple.query_
            conn
            pgQuery
    sqQuery =
      [SQQQ.sql|
        SELECT id, name, description, assumptions, md.years
        from scenarios
                join (select json_group_array(year) as years, scenario_id
                      from (select distinct year, scenario_id from metric_data order by year) as a
                      group by scenario_id) as md on scenarios.id = md.scenario_id      
      |]
    pgQuery =
      [PGQQ.sql|
        SELECT id, name, description, assumptions, md.years
        from scenarios
                join (select json_agg(year) as years, scenario_id
                      from (select distinct year, scenario_id from metric_data order by year) as a
                      group by scenario_id) as md on scenarios.id = md.scenario_id      
      |]

metricsDBForYear ::
  DBTypes.DatabaseEngine a ->
  Integer ->
  ScenarioTypes.Year ->
  Servant.Handler [ScenarioTypes.MetricData]
metricsDBForYear dbEngine scenarioId year =
  MonadTrans.liftIO $
    action
  where
    action =
      case dbEngine of
        DBTypes.SQLite3 conns -> Pool.withResource conns $ \conn ->
          SQLiteSimple.query
            conn
            sqQuery
            (scenarioId, year)
        DBTypes.PostgreSQL conns -> Pool.withResource conns $ \conn ->
          PGSimple.query
            conn
            pgQuery
            (scenarioId, year)
    sqQuery =
      [SQQQ.sql|
            SELECT metric_data.id,
                  scenario_id,
                  metric_id,
                  m.name,
                  m.description,
                  m.low_outcome,
                  m.low_outcome_text,
                  m.high_outcome,
                  m.high_outcome_text,
                  m.bins,
                  m.unit,
                  metric_data.year,
                  value,
                  spatial_data.spatial_values
            from metric_data
                    join (select id, name, description, low_outcome, low_outcome_text, high_outcome,high_outcome_text, json(bins) as bins, unit from metrics group by id) as m on metric_data.metric_id = m.id
                    left join (select scenario_1_zonal.metric_id as id,
                                      json_group_array(
                                              json_object('id', zone, 'value', value)
                                          )                      as spatial_values, year
                              from scenario_1_zonal
                              where year = 2016
                              group by metric_id) as spatial_data on metric_data.metric_id = spatial_data.id
            where scenario_id = ? and metric_data.year = ?
            order by metric_id;
            |]
    pgQuery =
      [PGQQ.sql|
            SELECT metric_data.id,
                  scenario_id,
                  metric_id,
                  m.name,
                  m.description,
                  m.low_outcome,
                  m.high_outcome,
                  m.bins,
                  m.unit
                  metric_data.year,
                  value,
                  spatial_data.spatial_values
            from metric_data
                    join (select id, name, description,low_outcome, low_outcome_text, high_outcome,high_outcome_text, json(bins) as bins, unit from metrics group by id) as m on metric_data.metric_id = m.id
                    left join (select scenario_1_zonal.metric_id as id,
                                      json_agg(
                                              json_object('id', zone, 'value', value)
                                          )                      as spatial_values, year
                              from scenario_1_zonal
                              where year = 2016
                              group by metric_id) as spatial_data on metric_data.metric_id = spatial_data.id
            where scenario_id = ? and metric_data.year = ?
            order by metric_id;
            |]

-- spatialDataDB ::
--   DBTypes.DatabaseEngine a ->
--   Integer ->
--   Integer ->
--   Servant.Handler Text.Text
-- spatialDataDB dbEngine scenarioId metricId =
--   MonadTrans.liftIO $
--     action
--   where
--     action =
--       case dbEngine of
--         DBTypes.SQLite3 conns -> Pool.withResource conns $ \conn ->
--           SQLiteSimple.query
--             conn
--             sqQuery
--             (scenarioId, metricId)
--         DBTypes.PostgreSQL conns -> Pool.withResource conns $ \conn ->
--           PGSimple.query
--             conn
--             pgQuery
--             (scenarioId, metricId)
--     sqQuery =
--       [SQQQ.sql|
--             SELECT metric_data.id,
--                   scenario_id,
--                   metric_id,
--                   m.name,
--                   m.description,
--                   metric_data.year,
--                   value,
--                   spatial_data.spatial_values as spatial_values
--             from metric_data
--                     join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
--                     left join (select scenario_1_zonal.metric_id as id,
--                                       json_group_array(
--                                               json_object('id', zone, 'value', value)
--                                       )                      as spatial_values, year
--                           from scenario_1_zonal
--                           group by metric_id) as spatial_data on metric_data.metric_id = spatial_data.id and metric_data.year = spatial_data.year
--         where scenario_id = ? and metric_id = ?;
--         |]
-- pgQuery =
--   [PGQQ.sql|
--         SELECT metric_data.id,
--               scenario_id,
--               metric_id,
--               m.name,
--               m.description,
--               metric_data.year,
--               value,
--               spatial_data.spatial_values as spatial_values
--         from metric_data
--                 join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
--                 left join (select scenario_1_zonal.metric_id as id,
--                                   json_agg(
--                                           json_object('id', zone, 'value', value)
--                                       )                      as spatial_values, year
--                           from scenario_1_zonal
--                           group by metric_id) as spatial_data on metric_data.metric_id = spatial_data.id and metric_data.year = spatial_data.year
--         where scenario_id = ? and metric_id = ?;
--         |]

tilesDB ::
  Mbtiles.MbtilesPool ->
  Int ->
  Int ->
  Text.Text ->
  Servant.Handler BS.ByteString
tilesDB conns z x stringY
  | (".mvt" `Text.isSuffixOf` stringY) || (".pbf" `Text.isSuffixOf` stringY) || (".vector.pbf" `Text.isSuffixOf` stringY) = getAnything conns z x stringY
  | otherwise = Servant.throwError $ Servant.err400 {Servant.errBody = "Unknown request: " <> ByteStringLazyChar8.fromStrict (TextEncoding.encodeUtf8 stringY)}

getAnything ::
  Mbtiles.MbtilesPool ->
  Int ->
  Int ->
  Text.Text ->
  Servant.Handler BS.ByteString
getAnything conns z x stringY =
  case getY stringY of
    Left e -> Servant.throwError $ Servant.err400 {Servant.errBody = "Unknown request: " <> ByteStringLazyChar8.fromStrict (TextEncoding.encodeUtf8 stringY)}
    Right (y, _) -> getTile conns z x y
  where
    getY s = TextRead.decimal $ Text.takeWhile Char.isNumber s

getTile :: Mbtiles.MbtilesPool -> Int -> Int -> Int -> Servant.Handler BS.ByteString
getTile conns z x y = do
  res <- MonadTrans.liftIO $ action
  case res of
    Just a ->
      return a
    _ ->
      Servant.throwError Servant.err404 {Servant.errBody = Errors.errorString "404" "No tiles found" "Try requesting a different tile."}
  where
    action =
      Mbtiles.runMbtilesPoolT conns (Mbtiles.getTile (Mbtiles.Z z) (Mbtiles.X x) (Mbtiles.Y y))
