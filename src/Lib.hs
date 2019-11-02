
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib
  ( startApp
  )
where

import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Servant
import qualified Data.Proxy as Proxy
import qualified Web.Browser as Browser

server
  :: Server API
server = Servant.serveDirectoryFileServer "static"


cookieApi :: Proxy.Proxy API
cookieApi = Proxy.Proxy
  
type API = Unprotected

type Unprotected = Raw

debug :: Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (requestHeaders req)
  app req resp

startApp :: IO ()
startApp = do
  b <- Browser.openBrowser "http://localhost:7249/"
  if b then
    run 7249 $ debug $ serve cookieApi server
  else
    print "Failed to start browser"