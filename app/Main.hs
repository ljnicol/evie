{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Config
import qualified Lib
import qualified Options.Generic as OptionsGeneric

main :: IO ()
main = OptionsGeneric.getRecord "servant-auth0" >>= doIt

doIt :: FilePath -> IO ()
doIt appDirectory = do
  config <- Config.getConfig appDirectory
  Lib.startApp config
