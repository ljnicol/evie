module DB where

import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Servant
import qualified Types.Scenario as ScenarioTypes

scenarioDB ::
  Pool.Pool PGSimple.Connection ->
  Int ->
  Servant.Handler ScenarioTypes.Scenario
scenarioDB conns scenarioId = do
  res <- MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
    PGSimple.query
      conn
      "SELECT id, name, description, assumptions, spatial_table, md.years from scenarios join (select json_agg(year) as years, scenario_id from (select distinct year, scenario_id from metric_data order by year) as a group by scenario_id ) as md on scenarios.id = md.scenario_id where scenario_id = ?"
      (PGSimple.Only scenarioId)
  case res of
    x : xs ->
      return x
    _ ->
      Servant.throwError Servant.err401 {Servant.errBody = "No results found"}

scenariosDB ::
  Pool.Pool PGSimple.Connection ->
  Servant.Handler [ScenarioTypes.Scenario]
scenariosDB conns =
  MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
    PGSimple.query_
      conn
      "SELECT id, name, description, assumptions, spatial_table, md.years from scenarios join (select json_agg(year) as years, scenario_id from (select distinct year, scenario_id from metric_data order by year) as a group by scenario_id ) as md on scenarios.id = md.scenario_id"

metricsDB ::
  Pool.Pool PGSimple.Connection ->
  Int ->
  Servant.Handler [ScenarioTypes.MetricData]
metricsDB conns scenarioId = MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query
    conn
    "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? order by metric_id"
    (PGSimple.Only scenarioId)

metricsDBForYear ::
  Pool.Pool PGSimple.Connection ->
  Int ->
  Int ->
  Servant.Handler [ScenarioTypes.MetricData]
metricsDBForYear conns scenarioId year = MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query
    conn
    "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? and year = ? order by metric_id"
    (scenarioId, year)

metricDB ::
  Pool.Pool PGSimple.Connection ->
  Int ->
  Int ->
  Servant.Handler ScenarioTypes.MetricData
metricDB conns scenarioId year = do
  res <- MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
    PGSimple.query
      conn
      "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? and year = ? order by metric_id"
      (scenarioId, year)
  case res of
    x : xs ->
      return x
    _ ->
      Servant.throwError Servant.err401 {Servant.errBody = "No results found"}
