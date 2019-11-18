module Controller where

import qualified DB
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as SQLiteSimple
import qualified Routes
import qualified Servant
import qualified Template
import qualified Types.Config as Config

server ::
  Config.Config -> Pool.Pool SQLiteSimple.Connection -> Servant.Server Routes.API
server config@Config.Config {..} conns = apiServer conns Servant.:<|> appServer conns
  where
    apiServer conns = DB.scenariosDB conns Servant.:<|> DB.metricsDB conns Servant.:<|> DB.getScenarioComparisonListDB conns
    appServer conns =
      Template.scenarioDetail conns (_configDirectory ++ "/" ++ "templates/scenario_detail.html")
        Servant.:<|> Template.scenarioComparison conns (_configDirectory ++ "/" ++ "templates/scenario_comparison.html")
        Servant.:<|> Template.scenarioDetailMap conns (_configDirectory ++ "/" ++ "templates/scenario_detail_map.html")
        Servant.:<|> Template.scenarioComparisonMap conns (_configDirectory ++ "/" ++ "templates/scenario_comparison_map.html")
        Servant.:<|> Servant.serveDirectoryFileServer (_configDirectory ++ "/" ++ "static")
