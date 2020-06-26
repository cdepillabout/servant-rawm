{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant.API (Get, JSON, (:>), (:<|>))

import Servant.RawM (RawM)

type Api = OtherEndpoint1 :<|> RawEndpoint :<|> OtherEndpoint2

type OtherEndpoint1 = "other-endpoint-1" :> Get '[JSON] Int

type RawEndpoint = "serve-directory" :> RawM

type OtherEndpoint2 = "other-endpoint-2" :> Get '[JSON] Int

-- | The port to run the server on.
port :: Int
port = 8201
