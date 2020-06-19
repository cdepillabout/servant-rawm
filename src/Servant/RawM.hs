{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{- |
Module      :  Servant.RawM

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)

This module exposes a 'RawM' type that allows you to embed a WAI
'Network.Wai.Application' in your Servant API.

It is similar to 'Servant.API.Raw.Raw' provided by Servant, but there is one big
difference. 'RawM' allows you to use monadic effects to create the
'Network.Wai.Application'.

What does this look like in practice?  The following is an example of using
'RawM':

@
  type Api = \"serve-directory-example\" :> 'RawM'

  serverRoot :: 'Servant.Server.ServerT' Api ('Control.Monad.Reader.ReaderT' 'FilePath' 'IO')
  serverRoot = rawEndpoint

  rawEndpoint :: 'Control.Monad.Reader.ReaderT' 'FilePath' 'IO' 'Network.Wai.Application'
  rawEndpoint = do
    filePath <- 'Control.Monad.Reader.ask'
    'serveDirectoryWebApp' filePath

  apiProxy :: 'Data.Proxy.Proxy' Api
  apiProxy = 'Data.Proxy.Proxy'

  app :: FilePath -> 'Network.Wai.Application'
  app filePath =
    'Servant.Server.serve' apiProxy apiServer
    where
      apiServer :: 'Servant.Server.Server' Api
      apiServer = 'Servant.Server.hoistServer' apiProxy transformation serverRoot

      transformation :: 'Control.Monad.Reader.ReaderT' 'FilePath' 'IO' a -> 'Servant.Server.Handler' a
      transformation readerT = 'Control.Monad.IO.Class.liftIO' $ 'Control.Monad.Reader.runReaderT' readerT filePath
@

Notice how the above @rawEndpoint@ handler is able to get the @filePath@ from
the 'Control.Monad.Reader.ReaderT'. Using Servant's default
'Servant.API.Raw.Raw' type, @rawEndpoint@ would have to be written like the
following:

@
  type Api\' = \"serve-directory-example\" :> 'Raw'

  serverRoot\' :: 'Server.ServerT' Api\' ('Control.Monad.Reader.ReaderT' 'FilePath' 'IO')
  serverRoot\' = rawEndpoint\'

  rawEndpoint\' :: 'Data.Tagged.Tagged' ('Control.Monad.Reader.ReaderT' 'FilePath' 'IO') 'Network.Wai.Application'
  rawEndpoint\' = ...
@

@rawEndpoint\'@ does not have access to the 'Control.Monad.Reader.ReaderT' monad,
so there is no way to get the directory path.

'RawM' solves this problem by allowing the 'Network.Wai.Application' to be
produced monadically.

There is an
<https://github.com/cdepillabout/servant-rawm/tree/master/example example> in
the source code repository that shows a more in-depth server, client, and
documentation.
-}

module Servant.RawM
  (
  -- * 'RawM' API parameter
    RawM
  , RawM'
  , FileServer
  -- * Helper functions for writing simple file servers
  , serveDirectoryWebApp
  , serveDirectoryFileServer
  , serveDirectoryWebAppLookup
  , serveDirectoryEmbedded
  , serveDirectoryWith
  ) where

import Servant.RawM.API
import Servant.RawM.Client ()
import Servant.RawM.Docs ()
import Servant.RawM.Server
