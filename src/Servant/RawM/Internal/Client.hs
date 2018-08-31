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

import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Client (Response)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (Header, Method)
import Servant.Client (Client, ClientM, HasClient(clientWithRoute))
import Servant.Client.Core (RunClient, Request)

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
  type Client m (RawM' serverType) =
        Method
    -> (Request -> Request)
    -> ClientM (Int, ByteString, MediaType, [Header], Response ByteString)

  clientWithRoute = undefined
  -- clientWithRoute :: Proxy (RawM' serverType) -> Request -> Client m (RawM' serverType)
  -- clientWithRoute Proxy req method f = performRequest method $ f req
