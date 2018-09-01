{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.RawM.Internal.Client

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module only exports a 'HasClient' instance for 'RawM'.
-}

module Servant.RawM.Internal.Client where

import Data.Proxy (Proxy(Proxy))
import Servant.Client (Client, ClientM, HasClient(clientWithRoute))
import Servant.Client.Core (runRequest, RunClient, Request, Response)
import Servant.RawM.Internal.API (RawM')

-- | Creates a client route like the following:
--
-- >>> :set -XTypeOperators
-- >>> import Data.Type.Equality ((:~:)(Refl))
-- >>> Refl :: Client (RawM' a) :~: (Method -> (Request -> Request) -> ClientM (Int, ByteString, MediaType, [Header], Response ByteString))
-- Refl
--
-- This allows modification of the underlying 'Request' to work for any sort of
-- 'Network.Wai.Application'.
--
-- Check out the
-- <https://github.com/cdepillabout/servant-rawm/tree/master/example example> in
-- the source code repository that shows a more in-depth server, client, and
-- documentation.
instance RunClient m => HasClient m (RawM' serverType) where
  type Client m (RawM' serverType) = (Request -> Request) -> ClientM Response

  clientWithRoute
    :: Proxy m
      -> Proxy (RawM' serverType)
      -> Request
      -> (Request -> Request)
      -> ClientM Response

  clientWithRoute Proxy Proxy r f = runRequest (f r)

  -- hoistClientMethod = undefined
