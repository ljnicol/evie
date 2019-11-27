module Controller where

import qualified DB
import qualified Routes
import qualified Servant
import qualified Template
import qualified Types.Config as Config
import qualified Types.DB as DB

server ::
  Config.Config -> DB.DatabaseEngine a -> Servant.Server Routes.API
server config@Config.Config {..} dbEngine =
  apiServer dbEngine Servant.:<|> appServer dbEngine Servant.:<|> Servant.serveDirectoryFileServer (_configSpatialDirectory)
  where
    apiServer dbEngine = DB.scenariosDB dbEngine Servant.:<|> DB.metricsDB dbEngine Servant.:<|> DB.getScenarioComparisonListDB dbEngine
    appServer dbEngine =
      Template.scenarioDetail dbEngine (_configTemplates ++ "/" ++ "scenario_detail.html")
        Servant.:<|> Template.scenarioComparison dbEngine (_configTemplates ++ "/" ++ "scenario_comparison.html")
        Servant.:<|> Template.scenarioDetailMap dbEngine (_configTemplates ++ "/" ++ "scenario_detail_map.html")
        Servant.:<|> Template.scenarioComparisonMap dbEngine (_configTemplates ++ "/" ++ "scenario_comparison_map.html")
        Servant.:<|> Servant.serveDirectoryFileServer (_configStaticDirectory)
