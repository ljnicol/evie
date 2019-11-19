{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module DB where

import Control.Monad (mapM)
import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.SqlQQ as PGQQ
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Database.SQLite.Simple.QQ as SQQQ
import qualified Errors
import qualified Servant
import qualified Types.DB as DBTypes
import qualified Types.Scenario as ScenarioTypes

--

getScenarioDetailDB :: DBTypes.DatabaseEngine a -> Integer -> ScenarioTypes.Year -> Servant.Handler ScenarioTypes.TemplateData
getScenarioDetailDB dbEngine scenarioId year = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ metricsDBForYear dbEngine scenarioId year
  scenario <- scenarioDB dbEngine scenarioId
  return $ ScenarioTypes.TemplateData metrics scenario

getScenarioMapDB :: DBTypes.DatabaseEngine a -> Integer -> Integer -> ScenarioTypes.Year -> Servant.Handler ScenarioTypes.TemplateData
getScenarioMapDB dbEngine scenarioId metricId year = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ metricDBForYear dbEngine scenarioId metricId year
  scenario <- scenarioDB dbEngine scenarioId
  return $ ScenarioTypes.TemplateData metrics scenario

getScenarioComparisonListDB :: DBTypes.DatabaseEngine a -> [Integer] -> ScenarioTypes.Year -> Servant.Handler [ScenarioTypes.TemplateData]
getScenarioComparisonListDB dbEngine scenarioIds year =
  let toTD :: ScenarioTypes.Year -> ScenarioTypes.Scenario -> Servant.Handler ScenarioTypes.TemplateData
      toTD year scenario = do
        metrics <- fmap ScenarioTypes.metricListToHashMap $ metricsDBForYear dbEngine (ScenarioTypes.scenarioId scenario) year
        pure $ ScenarioTypes.TemplateData metrics scenario
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
        SELECT id, name, description, assumptions, spatial_table, md.years 
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
        SELECT id, name, description, assumptions, spatial_table, md.years 
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
        SELECT id, name, description, assumptions, spatial_table, md.years
        from scenarios
                join (select json_group_array(year) as years, scenario_id
                      from (select distinct year, scenario_id from metric_data order by year) as a
                      group by scenario_id) as md on scenarios.id = md.scenario_id      
      |]
    pgQuery =
      [PGQQ.sql|
        SELECT id, name, description, assumptions, spatial_table, md.years
        from scenarios
                join (select json_agg(year) as years, scenario_id
                      from (select distinct year, scenario_id from metric_data order by year) as a
                      group by scenario_id) as md on scenarios.id = md.scenario_id      
      |]

metricsDB ::
  DBTypes.DatabaseEngine a ->
  Integer ->
  Servant.Handler [ScenarioTypes.MetricData]
metricsDB dbEngine scenarioId =
  MonadTrans.liftIO $
    action
  where
    action =
      case dbEngine of
        DBTypes.SQLite3 conns -> Pool.withResource conns $ \conn ->
          SQLiteSimple.query
            conn
            sqQuery
            (SQLiteSimple.Only scenarioId)
        DBTypes.PostgreSQL conns -> Pool.withResource conns $ \conn ->
          PGSimple.query
            conn
            pgQuery
            (PGSimple.Only scenarioId)
    sqQuery =
      [SQQQ.sql|
              SELECT metric_data.id,
                      scenario_id,
                      metric_id,
                      m.name,
                      m.description,
                      year,
                      value,
                      spatial_table_column
              from metric_data
                        join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
              where scenario_id = ?
              order by metric_id  
          |]
    pgQuery =
      [PGQQ.sql|
              SELECT metric_data.id,
                      scenario_id,
                      metric_id,
                      m.name,
                      m.description,
                      year,
                      value,
                      spatial_table_column
              from metric_data
                        join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
              where scenario_id = ?
              order by metric_id  
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
                    year,
                    value,
                    spatial_table_column
            from metric_data
                      join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
            where scenario_id = ?
              and year = ?
            order by metric_id
            |]
    pgQuery =
      [PGQQ.sql|
            SELECT metric_data.id,
                    scenario_id,
                    metric_id,
                    m.name,
                    m.description,
                    year,
                    value,
                    spatial_table_column
            from metric_data
                      join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
            where scenario_id = ?
              and year = ?
            order by metric_id
            |]

metricDBForYear ::
  DBTypes.DatabaseEngine a ->
  Integer ->
  Integer ->
  ScenarioTypes.Year ->
  Servant.Handler [ScenarioTypes.MetricData]
metricDBForYear dbEngine scenarioId metricId year =
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
                    year,
                    value,
                    spatial_table_column
            from metric_data
                      join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
            where scenario_id = ?
              and metric_data.metric_id = ?
              and year = ?
            order by metric_id
              |]
    pgQuery =
      [PGQQ.sql|
            SELECT metric_data.id,
                    scenario_id,
                    metric_id,
                    m.name,
                    m.description,
                    year,
                    value,
                    spatial_table_column
            from metric_data
                      join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
            where scenario_id = ?
              and metric_data.metric_id = ?
              and year = ?
            order by metric_id
              |]

metricDB ::
  DBTypes.DatabaseEngine a ->
  Integer ->
  ScenarioTypes.Year ->
  Servant.Handler ScenarioTypes.MetricData
metricDB dbEngine scenarioId year = do
  res <-
    MonadTrans.liftIO $ action
  case res of
    x : xs ->
      return x
    _ ->
      Servant.throwError Servant.err401 {Servant.errBody = Errors.errorString "401" "No results found" "Try a different metric or contact your support team."}
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
                year,
                value,
                spatial_table_column
          from metric_data
                  join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
          where scenario_id = ?
          and year = ?
          order by metric_id
        |]
    pgQuery =
      [PGQQ.sql| 
          SELECT metric_data.id,
                scenario_id,
                metric_id,
                m.name,
                m.description,
                year,
                value,
                spatial_table_column
          from metric_data
                  join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id
          where scenario_id = ?
          and year = ?
          order by metric_id
        |]
