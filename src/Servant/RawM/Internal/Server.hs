{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.RawM.Internal.Server

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports 'HasServer' instances for 'Throws' and 'Throwing'.
-}

module Servant.RawM.Internal.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Proxy (Proxy(Proxy))
import Network.Wai
       (Application, Request, Response, ResponseReceived)
import Network.Wai.Application.Static
       (defaultWebAppSettings, staticApp)
import Servant (Context, HasServer(route), ServerT, runHandler)
import qualified Servant as Servant
import Servant.Server.Internal
       (Delayed, Router'(RawRouter), RouteResult(Fail, FailFatal, Route), responseServantErr,
        runDelayed)
-- import System.FilePath (addTrailingPathSeparator)

import Servant.RawM.Internal.API (RawM)

instance HasServer RawM context where
  type ServerT RawM m = m Application
  route
    :: forall env.
       Proxy RawM
    -> Context context
    -> Delayed env (Servant.Handler Application)
    -> Router' env (Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived)
  route Proxy _ rawApplication = RawRouter go
    where
      go
        :: env
        -> Request
        -> (RouteResult Response -> IO ResponseReceived)
        -> IO ResponseReceived
      go env request respond =
        runResourceT $ do
          routeRes <- runDelayed rawApplication env request
          liftIO $
            case routeRes of
              (Fail e) -> respond $ Fail e
              (FailFatal e) -> respond $ FailFatal e
              (Route handlerApp) -> do
                eitherApp <- runHandler handlerApp
                case eitherApp of
                  Left err -> respond . Route $ responseServantErr err
                  Right app -> app request (respond . Route)
