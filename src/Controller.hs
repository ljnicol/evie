module Controller where

import qualified DB
import qualified Data.Text as Text
import qualified Database.Mbtiles as Mbtiles
import qualified Routes
import qualified Servant
import qualified Template
import qualified Types.Config as Config
import qualified Types.DB as DB

server ::
  Config.Config -> Mbtiles.MbtilesPool -> DB.DatabaseEngine a -> Servant.Server Routes.API
server config@Config.Config {..} spatialConns dbEngine =
  apiServer dbEngine Servant.:<|> appServer dbEngine Servant.:<|> DB.tilesDB spatialConns Servant.:<|> Template.renderStatic (_configTemplates ++ "/" ++ "index.html")
  where
    apiServer dbEngine =
      DB.scenariosDB dbEngine
        Servant.:<|> DB.getScenarioDetailDBForYear dbEngine
        Servant.:<|> DB.getScenarioDetailDB dbEngine
        Servant.:<|> DB.getScenarioComparisonListDB dbEngine
        Servant.:<|> DB.getScenarioComparisonMetrics dbEngine
    host = Text.unpack _configApplicationDomain ++ ":" ++ (show _configApplicationPort)
    appServer dbEngine =
      Template.scenarioDetail dbEngine host (_configTemplates ++ "/" ++ "scenario_detail.html")
        Servant.:<|> Template.scenarioComparison dbEngine host (_configTemplates ++ "/" ++ "scenario_comparison.html")
        Servant.:<|> Template.scenarioDetailMap dbEngine host (_configTemplates ++ "/" ++ "scenario_detail_map.html")
        Servant.:<|> Template.scenarioComparisonMap dbEngine host (_configTemplates ++ "/" ++ "scenario_comparison_map.html")
        Servant.:<|> Servant.serveDirectoryFileServer (_configLibDirectory)
        Servant.:<|> Servant.serveDirectoryFileServer (_configStaticDirectory)
