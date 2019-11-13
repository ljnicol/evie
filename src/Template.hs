module Template where

import qualified Control.Monad.Trans as MonadTrans (liftIO)
import qualified DB
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap, fromList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Servant
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import System.IO.Error (tryIOError)
import Text.Ginger (SourcePos, Template, VarName, easyRender, makeContextHtml, parseGingerFile, runGinger, toGVal)
import Text.Ginger.GVal (GVal, ToGVal)
import Text.Ginger.Html (htmlSource)
import qualified Types.Scenario as ScenarioTypes

-- GINGER

sampleContext :: Pool.Pool PGSimple.Connection -> Servant.Handler ScenarioTypes.TemplateData
sampleContext conns = do
  metrics <- fmap ScenarioTypes.metricListToHashMap $ DB.metricsDB conns 1
  scenario <- DB.scenarioDB conns 1
  return $ ScenarioTypes.TemplateData metrics scenario

-- Given a Template and a HashMap of context, render the template to Text
render :: Template SourcePos -> ScenarioTypes.TemplateData -> Text.Text
render template context = htmlSource $ easyRender context template

loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    loadFile :: FilePath -> IO String
    loadFile fn' = openFile fn' ReadMode >>= hGetContents

renderTemplate :: Pool.Pool PGSimple.Connection -> Servant.Handler Text.Text
renderTemplate conns = do
  template <- MonadTrans.liftIO $ parseGingerFile loadFileMay "base.html"
  case template of
    Left err -> return $ Text.pack $ show err
    Right template' -> do
      context <- sampleContext conns
      MonadTrans.liftIO $ print (Aeson.encode context)
      return $ render template' context
