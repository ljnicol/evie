{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = doIt

doIt :: IO ()
doIt = do
  startApp
