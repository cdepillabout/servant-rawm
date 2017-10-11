{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{- |
Module      :  Servant.RawM

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)

This "Servant.RawM" module exposes a 'RawM' type that allows you to embed a WAI
'Network.Wai.Application' in your Servant API.

It is similar to 'Raw' provided by Servant, but there is one big difference.
'RawM' allows you to use monadic effects to create the
'Network.Wai.Application'.

What does this look like in practice?  The following is an example of using
'RawM':

@
  type Api = \"serve-directory-example\" :> 'RawM'

  serverRoot :: 'Server.ServerT' Api ('Control.Monad.Reader.ReaderT' 'FilePath' 'IO')
  serverRoot = rawEndpoint

  rawEndpoint :: 'Control.Monad.Reader.ReaderT' 'FilePath' 'IO' 'Network.Wai.Application'
  rawEndpoint = do
    filePath <- 'Control.Monad.Reader.ask'
    'serveDirectoryWebApp' filePath

  app :: FilePath -> 'Network.Wai.Application'
  app filePath =
    'Servant.serve' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' Api) apiServer
    where
      apiServer :: Server Api
      apiServer = 'Servant.enter' ('Servant.NT' transformation) serverRoot

      transformation :: 'Control.Monad.Reader.ReaderT' 'FilePath' 'IO' a -> 'Servant.Handler' a
      transformation readerT = 'Control.Monad.IO.Class.liftIO' $ 'Control.Monad.Reader.runReaderT' readerT filePath

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

import Servant.RawM.Internal
