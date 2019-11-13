module Template where

import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified DB
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Servant
import qualified System.IO as SystemIO (IOMode (ReadMode), hGetContents, openFile)
import qualified System.IO.Error as IOError (tryIOError)
import qualified Text.Ginger as Ginger
import qualified Text.Ginger.Html as GingerHtml (htmlSource)
import qualified Types.Scenario as ScenarioTypes

-- Common

loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  IOError.tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    loadFile :: FilePath -> IO String
    loadFile fn' = SystemIO.openFile fn' SystemIO.ReadMode >>= SystemIO.hGetContents

renderPage :: FilePath -> (Ginger.Template Ginger.SourcePos -> Text.Text) -> Servant.Handler Text.Text
renderPage templateFile renderFn = do
  template <- MonadTrans.liftIO $ Ginger.parseGingerFile loadFileMay templateFile
  case template of
    Left err -> return $ Text.pack $ show err
    Right template' ->
      return $ renderFn template'

-- Scenario Detail Page

renderScenarioDetail :: Pool.Pool PGSimple.Connection -> FilePath -> Integer -> Servant.Handler Text.Text
renderScenarioDetail conns templateFile scenarioId = do
  context <- getScenarioDetailDB conns scenarioId
  renderPage templateFile (render context)

getScenarioDetailDB :: Pool.Pool PGSimple.Connection -> Integer -> Servant.Handler ScenarioTypes.TemplateData
getScenarioDetailDB conns scenarioId = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ DB.metricsDB conns scenarioId
  scenario <- DB.scenarioDB conns 1
  return $ ScenarioTypes.TemplateData metrics scenario

render :: ScenarioTypes.TemplateData -> Ginger.Template Ginger.SourcePos -> Text.Text
render context template = GingerHtml.htmlSource $ Ginger.easyRender context template

-- Scenario Comparison Page
renderScenarioComparison :: Pool.Pool PGSimple.Connection -> FilePath -> Integer -> Integer -> Servant.Handler Text.Text
renderScenarioComparison conns templateFile scenarioId1 scenarioId2 = do
  context <- getScenarioDetailDB conns scenarioId1
  renderPage templateFile (render context)

getScenarioComparisonDB :: Pool.Pool PGSimple.Connection -> Integer -> Servant.Handler ScenarioTypes.TemplateData
getScenarioComparisonDB conns scenarioId = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ DB.metricsDB conns scenarioId
  scenario <- DB.scenarioDB conns 1
  return $ ScenarioTypes.TemplateData metrics scenario

-- Scenario Map Page

renderScenarioDetailMap :: Pool.Pool PGSimple.Connection -> FilePath -> Integer -> Integer -> Servant.Handler Text.Text
renderScenarioDetailMap conns templateFile scenarioId metricId = do
  context <- getScenarioDetailMapDB conns scenarioId
  renderPage templateFile (render context)

getScenarioDetailMapDB :: Pool.Pool PGSimple.Connection -> Integer -> Servant.Handler ScenarioTypes.TemplateData
getScenarioDetailMapDB conns scenarioId = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ DB.metricsDB conns scenarioId
  scenario <- DB.scenarioDB conns 1
  return $ ScenarioTypes.TemplateData metrics scenario

-- Scenario Comparison Map

renderScenarioComparisonMap :: Pool.Pool PGSimple.Connection -> FilePath -> Integer -> Integer -> Integer -> Servant.Handler Text.Text
renderScenarioComparisonMap conns templateFile scenarioId1 scenarioId2 metricId = do
  context <- getScenarioDetailDB conns scenarioId1
  renderPage templateFile (render context)

getScenarioComparisonMapDB :: Pool.Pool PGSimple.Connection -> Integer -> Servant.Handler ScenarioTypes.TemplateData
getScenarioComparisonMapDB conns scenarioId = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ DB.metricsDB conns scenarioId
  scenario <- DB.scenarioDB conns 1
  return $ ScenarioTypes.TemplateData metrics scenario
