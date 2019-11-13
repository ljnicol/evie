module Template where

import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified DB
import qualified Data.Aeson as Aeson
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Servant
import qualified System.IO as SystemIO (IOMode (ReadMode), hGetContents, openFile)
import qualified System.IO.Error as IOError (tryIOError)
import qualified Text.Ginger as Ginger
import qualified Text.Ginger.Html as GingerHtml (htmlSource)
import qualified Types.Scenario as ScenarioTypes

sampleContext :: Pool.Pool PGSimple.Connection -> Integer -> Servant.Handler ScenarioTypes.TemplateData
sampleContext conns scenarioId = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ DB.metricsDB conns scenarioId
  scenario <- DB.scenarioDB conns 1
  return $ ScenarioTypes.TemplateData metrics scenario

render :: Ginger.Template Ginger.SourcePos -> ScenarioTypes.TemplateData -> Text.Text
render template context = GingerHtml.htmlSource $ Ginger.easyRender context template

loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  IOError.tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    loadFile :: FilePath -> IO String
    loadFile fn' = SystemIO.openFile fn' SystemIO.ReadMode >>= SystemIO.hGetContents

renderTemplate :: Pool.Pool PGSimple.Connection -> FilePath -> Integer -> Servant.Handler Text.Text
renderTemplate conns templateDirectory scenarioId = do
  template <- MonadTrans.liftIO $ Ginger.parseGingerFile loadFileMay (templateDirectory ++ "/" ++ "base.html")
  case template of
    Left err -> return $ Text.pack $ show err
    Right template' -> do
      context <- sampleContext conns scenarioId
      MonadTrans.liftIO $ print (Aeson.encode context)
      return $ render template' context
