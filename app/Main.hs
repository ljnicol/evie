{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib
import qualified Config
import qualified Options.Generic as OptionsGeneric

main :: IO ()
main = OptionsGeneric.getRecord "servant-auth0" >>= doIt

doIt :: FilePath -> IO ()
doIt configFilePath = do
  config <- Config.getConfig configFilePath 
  Lib.startApp config
