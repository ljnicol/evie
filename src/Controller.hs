module Controller where

import qualified DB
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Routes
import qualified Servant
import qualified Template
import qualified Types.Config as Config

server ::
  Config.Config -> Pool.Pool PGSimple.Connection -> Servant.Server Routes.API
server config@Config.Config {..} conns = apiServer conns Servant.:<|> appServer conns
  where
    apiServer conns = DB.scenariosDB conns Servant.:<|> DB.metricsDB conns
    appServer conns = Template.renderTemplate conns (_configDirectory ++ "/" ++ "templates") Servant.:<|> Servant.serveDirectoryFileServer (_configDirectory ++ "/" ++ "static")
