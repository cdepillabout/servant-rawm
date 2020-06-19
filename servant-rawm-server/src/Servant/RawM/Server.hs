{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.RawM.Server

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports 'HasServer' instances for 'RawM'', as well as some helper
functions for serving directories of files.
-}

module Servant.RawM.Server (
  -- * Reexport RawM API
  module Servant.RawM.API,
  -- * Helper functions for writing simple file servers
  serveDirectoryWebApp,
  serveDirectoryFileServer,
  serveDirectoryWebAppLookup,
  serveDirectoryEmbedded,
  serveDirectoryWith
)
  where

import Control.Monad.IO.Class          (liftIO)
import Control.Monad.Trans.Resource    (runResourceT)
import Data.ByteString                 (ByteString)
import Data.Proxy                      (Proxy (Proxy))
import Network.Wai                     (Application, Request, Response,
                                        ResponseReceived)
import Network.Wai.Application.Static  (StaticSettings,
                                        defaultFileServerSettings,
                                        defaultWebAppSettings, embeddedSettings,
                                        staticApp, webAppSettingsWithLookup)
import Servant                         (Context, Handler, HasServer (hoistServerWithContext, route),
                                        ServerT, runHandler)
import Servant.Server.Internal         (Delayed,
                                        RouteResult (Fail, FailFatal, Route),
                                        Router' (RawRouter),
                                        responseServerError, runDelayed)
import System.FilePath                 (addTrailingPathSeparator)
import WaiAppStatic.Storage.Filesystem (ETagLookup)

import Servant.RawM.API

-- | Creates a server instance like the following:
--
--
-- >>> :set -XTypeOperators
-- >>> import Data.Type.Equality ((:~:)(Refl))
-- >>> Refl :: ServerT (RawM' a) m :~: m Application
-- Refl
instance HasServer (RawM' serverType) context where
  type ServerT (RawM' serverType) m = m Application
  route
    :: forall env.
       Proxy (RawM' serverType)
    -> Context context
    -> Delayed env (Handler Application)
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
                  Left err  -> respond . Route $ responseServerError err
                  Right app -> app request (respond . Route)

  hoistServerWithContext
    :: Proxy (RawM' serverType)
      -> Proxy context
      -> (forall x. m x -> n x)
      -> m Application
      -> n Application
  hoistServerWithContext Proxy Proxy f m = f m

-- | Serve anything under the specified directory as a 'RawM'' endpoint.
--
-- @
-- type MyApi = "static" :> RawM'
--
-- server :: ServerT MyApi m
-- server = serveDirectoryWebApp "\/var\/www"
-- @
--
-- would capture any request to @\/static\/\<something>@ and look for
-- @\<something>@ under @\/var\/www@.
--
-- It will do its best to guess the MIME type for that file, based on the extension,
-- and send an appropriate /Content-Type/ header if possible.
--
-- If your goal is to serve HTML, CSS and Javascript files that use the rest of the API
-- as a webapp backend, you will most likely not want the static files to be hidden
-- behind a /\/static\// prefix. In that case, remember to put the 'serveDirectoryWebApp'
-- handler in the last position, because /servant/ will try to match the handlers
-- in order.
--
-- Corresponds to the `defaultWebAppSettings` `StaticSettings` value.
serveDirectoryWebApp :: Applicative m => FilePath -> ServerT (RawM' serverType) m
serveDirectoryWebApp = serveDirectoryWith . defaultWebAppSettings . addTrailingPathSeparator

-- | Same as 'serveDirectoryWebApp', but uses `defaultFileServerSettings`.
serveDirectoryFileServer :: Applicative m => FilePath -> ServerT (RawM' serverType) m
serveDirectoryFileServer = serveDirectoryWith . defaultFileServerSettings . addTrailingPathSeparator

-- | Same as 'serveDirectoryWebApp', but uses 'webAppSettingsWithLookup'.
serveDirectoryWebAppLookup :: Applicative m => ETagLookup -> FilePath -> ServerT (RawM' serverType) m
serveDirectoryWebAppLookup etag =
  serveDirectoryWith . flip webAppSettingsWithLookup etag . addTrailingPathSeparator

-- | Uses 'embeddedSettings'.
serveDirectoryEmbedded :: Applicative m => [(FilePath, ByteString)] -> ServerT (RawM' serverType) m
serveDirectoryEmbedded files = serveDirectoryWith (embeddedSettings files)

-- | Alias for 'staticApp'. Lets you serve a directory with arbitrary
-- 'StaticSettings'. Useful when you want particular settings not covered by
-- the four other variants. This is the most flexible method.
serveDirectoryWith :: Applicative m => StaticSettings -> ServerT (RawM' serverType) m
serveDirectoryWith = pure . staticApp
