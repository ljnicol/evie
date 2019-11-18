{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Init
import qualified Lib
import qualified Options.Generic as OptionsGeneric
import qualified Types.Config as Config

main :: IO ()
main = do
  commandLineOpts <- OptionsGeneric.unwrapRecord "evie"
  doIt commandLineOpts

doIt :: Config.CommandLine OptionsGeneric.Unwrapped -> IO ()
doIt commandLine = do
  config <- Init.getConfig commandLine
  Lib.startApp config
