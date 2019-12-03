module Template where

import Control.Monad (mapM)
import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified DB
import qualified Data.Text as Text
import qualified Servant
import qualified System.IO as SystemIO (IOMode (ReadMode), hGetContents, openFile)
import qualified System.IO.Error as IOError (tryIOError)
import qualified Text.Ginger as Ginger
import qualified Text.Ginger.Html as GingerHtml (htmlSource)
import qualified Types.DB as DBTypes
import qualified Types.Scenario as ScenarioTypes

-- Static File

renderStatic :: FilePath -> Servant.Handler Text.Text
renderStatic templateFile = do
  template <- MonadTrans.liftIO $ Ginger.parseGingerFile loadFileMay templateFile
  case template of
    Left err -> return $ Text.pack $ show err
    Right template' ->
      return $ GingerHtml.htmlSource $ Ginger.easyRender () template'

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

scenarioDetail :: DBTypes.DatabaseEngine a -> String -> FilePath -> Integer -> ScenarioTypes.Year -> Servant.Handler Text.Text
scenarioDetail conns host templateFile scenarioId year = do
  context <- DB.getScenarioDetailDB conns scenarioId year host
  renderPage templateFile (renderScenarioDetail context)

renderScenarioDetail :: ScenarioTypes.TemplateData -> Ginger.Template Ginger.SourcePos -> Text.Text
renderScenarioDetail context template = GingerHtml.htmlSource $ Ginger.easyRender context template

-- Scenario Comparison Page

scenarioComparison :: DBTypes.DatabaseEngine a -> String -> FilePath -> [Integer] -> [ScenarioTypes.Year] -> Servant.Handler Text.Text
scenarioComparison conns host templateFile scenarioIds years = do
  case (scenarioIds, years) of
    (s : _, y : _) -> do
      context <- fmap ScenarioTypes.ComparisonTemplateData $ mapM (\a -> DB.getScenarioDetailDB conns a y host) scenarioIds
      renderPage templateFile (renderScenarioComparison (context))
    _ ->
      return $ Text.pack $ show "Failed to parse scenario IDs and years"

renderScenarioComparison :: ScenarioTypes.ComparisonTemplateData -> Ginger.Template Ginger.SourcePos -> Text.Text
renderScenarioComparison context template = GingerHtml.htmlSource $ Ginger.easyRender context template
-- -- Scenario Map Page

-- scenarioDetailMap :: DBTypes.DatabaseEngine a -> FilePath -> Integer -> Integer -> ScenarioTypes.Year -> Servant.Handler Text.Text
-- scenarioDetailMap conns templateFile scenarioId metricId year = do
--   context <- DB.getScenarioMapDB conns scenarioId metricId year
--   renderPage templateFile (renderScenarioDetailMap context)

-- renderScenarioDetailMap :: ScenarioTypes.TemplateData -> Ginger.Template Ginger.SourcePos -> Text.Text
-- renderScenarioDetailMap context template = GingerHtml.htmlSource $ Ginger.easyRender context template

-- Scenario Comparison Map

-- scenarioComparisonMap :: DBTypes.DatabaseEngine a -> FilePath -> Integer -> Integer -> Integer -> ScenarioTypes.Year -> Servant.Handler Text.Text
-- scenarioComparisonMap conns templateFile scenarioId1 scenarioId2 metricId year = do
--   context <- DB.getScenarioMapDB conns scenarioId1 metricId year
--   renderPage templateFile (renderScenarioComparisonMap context)

-- renderScenarioComparisonMap :: ScenarioTypes.TemplateData -> Ginger.Template Ginger.SourcePos -> Text.Text
-- renderScenarioComparisonMap context template = GingerHtml.htmlSource $ Ginger.easyRender context template
