{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Config
import qualified Lib
import qualified Options.Generic as OptionsGeneric

main :: IO ()
main = OptionsGeneric.getRecord "servant-auth0" >>= doIt

doIt :: Maybe FilePath -> IO ()
doIt cmdLineAppDirectory = do
  config <- Config.getConfig cmdLineAppDirectory
  Lib.startApp config
