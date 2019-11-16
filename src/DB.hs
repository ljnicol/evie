module DB where

import Control.Monad (mapM)
import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Servant
import qualified Types.Scenario as ScenarioTypes

--

getScenarioDetailDB :: Pool.Pool PGSimple.Connection -> Integer -> Integer -> Servant.Handler ScenarioTypes.TemplateData
getScenarioDetailDB conns scenarioId year = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ metricsDBForYear conns scenarioId year
  scenario <- scenarioDB conns scenarioId
  return $ ScenarioTypes.TemplateData metrics scenario

getScenarioMapDB :: Pool.Pool PGSimple.Connection -> Integer -> Integer -> Integer -> Servant.Handler ScenarioTypes.TemplateData
getScenarioMapDB conns scenarioId metricId year = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ metricDBForYear conns scenarioId metricId year
  scenario <- scenarioDB conns scenarioId
  return $ ScenarioTypes.TemplateData metrics scenario

getScenarioComparisonListDB :: Pool.Pool PGSimple.Connection -> [Integer] -> Integer -> Servant.Handler [ScenarioTypes.TemplateData]
getScenarioComparisonListDB conns scenarioIds year =
  let toTD :: Integer -> ScenarioTypes.Scenario -> Servant.Handler ScenarioTypes.TemplateData
      toTD year scenario = do
        metrics <- fmap ScenarioTypes.metricListToHashMap $ metricsDBForYear conns (ScenarioTypes.scenarioId scenario) year
        pure $ ScenarioTypes.TemplateData metrics scenario
   in do
        scenarios <- scenariosSelectDB conns scenarioIds
        mapM (toTD year) scenarios

--

scenarioDB ::
  Pool.Pool PGSimple.Connection ->
  Integer ->
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

scenariosSelectDB ::
  Pool.Pool PGSimple.Connection ->
  [Integer] ->
  Servant.Handler [ScenarioTypes.Scenario]
scenariosSelectDB conns scenarioIds =
  MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
    PGSimple.query
      conn
      "SELECT id, name, description, assumptions, spatial_table, md.years from scenarios join (select json_agg(year) as years, scenario_id from (select distinct year, scenario_id from metric_data order by year) as a group by scenario_id ) as md on scenarios.id = md.scenario_id where scenario_id in ?"
      (PGSimple.Only $ PGSimple.In scenarioIds)

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
  Integer ->
  Servant.Handler [ScenarioTypes.MetricData]
metricsDB conns scenarioId = MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query
    conn
    "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? order by metric_id"
    (PGSimple.Only scenarioId)

metricsDBForYear ::
  Pool.Pool PGSimple.Connection ->
  Integer ->
  Integer ->
  Servant.Handler [ScenarioTypes.MetricData]
metricsDBForYear conns scenarioId year = MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query
    conn
    "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? and year = ? order by metric_id"
    (scenarioId, year)

metricDBForYear ::
  Pool.Pool PGSimple.Connection ->
  Integer ->
  Integer ->
  Integer ->
  Servant.Handler [ScenarioTypes.MetricData]
metricDBForYear conns scenarioId metricId year = MonadTrans.liftIO $ Pool.withResource conns $ \conn ->
  PGSimple.query
    conn
    "SELECT metric_data.id, scenario_id, metric_id, m.name, m.description, year, value, spatial_table_column from metric_data join (select id, name, description from metrics group by id) as m on metric_data.metric_id = m.id where scenario_id = ? and metric_data.metric_id = ? and year = ? order by metric_id"
    (scenarioId, metricId, year)

metricDB ::
  Pool.Pool PGSimple.Connection ->
  Integer ->
  Integer ->
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
