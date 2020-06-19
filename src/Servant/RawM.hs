{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{- |
Module      :  Servant.RawM

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)

This module exposes a 'RawM' type that allows you to embed a WAI
'Network.Wai.Application' in your Servant API. See the documentation of
"Servant.RawM.API" for detailed introduction.

After @servant-rawm@ 0.4.0.0, it is recommended to import sub-packages
@servant-rawm-api@, @servant-rawm-server@, @servant-rawm-client@, and
@servant-rawm-docs@ instead of @servant-rawm@ to avoid pulling in unnecessary
dependencies. This can reduce the compilation time dramatically.
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
