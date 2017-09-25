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

This module only exports 'HasClient' instances for 'Throws' and 'Throwing'.
-}

module Servant.RawM.Internal.Client where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Client (Response)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (Header, Method)
import Servant.Client (Client, ClientM, HasClient(clientWithRoute))
import Servant.Common.Req (Req, performRequest)

import Servant.RawM.Internal.API (RawM)

instance HasClient RawM where
  type Client RawM = Method -> (Req -> Req) -> ClientM (Int, ByteString, MediaType, [Header], Response ByteString)

  clientWithRoute :: Proxy RawM -> Req -> Client RawM
  clientWithRoute Proxy req method f = performRequest method $ f req
