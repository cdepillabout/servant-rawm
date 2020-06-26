{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Servant.RawM

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)

This module defines the 'RawM' type that allows you to embed a WAI
'Network.Wai.Application' in your Servant API.

It is similar to 'Servant.API.Raw.Raw' provided by Servant, but there is one big
difference. 'RawM' allows you to use monadic effects to create the
'Network.Wai.Application'.

What does this look like in practice?  The following is an example of using
'RawM':

@
  import "Servant.RawM.Server"

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
<https://github.com/cdepillabout/servant-rawm/tree/master/servant-rawm-examples-and-tests/example example>
in the source code repository that shows a more in-depth server, client, and
documentation.

After @servant-rawm@ 1.0.0.0, the implementations for 'RawM' server, client,
and documentation generator are divided into three packages:
<https://hackage.haskell.org/package/servant-rawm-server servant-rawm-server>,
<https://hackage.haskell.org/package/servant-rawm-client servant-rawm-client>,
and
<https://hackage.haskell.org/package/servant-rawm-client servant-rawm-docs>
to avoid pulling in unnecessary dependencies. This module is re-exported in
"Servant.RawM.Server", "Servant.RawM.Client", and "Servant.RawM.Docs", so you
don't need to import this module explicitly. Import the corresponding
implementation instead.
-}

module Servant.RawM (
  -- * 'RawM' API parameter
    RawM
  , RawM'
  , FileServer
)
  where

import Data.Typeable (Typeable)
import Servant.Links (HasLink, MkLink, toLink)

-- | Specialization of 'RawM'' to 'FileServer'. This can be used if you are
-- using 'Servant.RawM.serveDirectoryWebApp',
-- 'Servant.RawM.serveDirectoryFileServer', etc.
type RawM = RawM' FileServer

-- | This is a type to use to define a Servant API. It signifies a route that
-- allows embedding of a WAI 'Network.Wai.Application'. It is similar to
-- 'Servant.API.Raw.Raw', but it has a 'Servant.Server.HasServer' instance that
-- allows embedding of monadic effects. This should be more convenient than
-- 'Servant.API.Raw.Raw'.
--
-- The phantom type @serverType@ is used for defining the 'Servant.Docs.HasDocs'
-- instance. There are instances for 'Servant.Client.HasClient' and
-- 'Servant.Server.HasServer' for 'RawM'' with a polymorphic phantom type, but
-- there is only a 'Servant.Docs.HasDocs' instance specified for @'RawM''
-- 'FileServer'@. This allows the end-user to easily create a
-- 'Servant.Docs.HasDocs' instance for a custom 'Network.Wai.Application'.
data RawM' serverType deriving Typeable

instance HasLink (RawM' st) where
  type MkLink (RawM' st) a = a
  toLink toA _ = toA

-- | Used by 'RawM' as a phantom type.
data FileServer deriving Typeable
