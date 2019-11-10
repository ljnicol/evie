{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Init
import qualified Lib
import qualified Options.Generic as OptionsGeneric

main :: IO ()
main = OptionsGeneric.getRecord "servant-auth0" >>= doIt

doIt :: Maybe FilePath -> IO ()
doIt cmdLineAppDirectory = do
  config <- Init.getConfig cmdLineAppDirectory
  Lib.startApp config
