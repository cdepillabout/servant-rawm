{- |
Module      :  Servant.RawM.Internal.API

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)

-}

module Servant.RawM.Internal.API where

import Data.Typeable (Typeable)

-- | Specialization of 'RawM\'' to 'FileServer'. This can be used if you are
-- using 'Servant.RawM.serveDirectoryWebApp',
-- 'Servant.RawM.serveDirectoryFileServer', etc.
type RawM = RawM' FileServer

-- | This is a type to use to define a Servant API. It signifies a route that
-- allows embedding of a WAI 'Network.Wai.Application'. It is similar to
-- 'Servant.API.Raw.Raw', but it has a 'Servant.Server.HasServer' instance that
-- allows embedding of monad effects. This should be more convenient than
-- 'Servant.API.Raw.Raw'.
--
-- The phantom type @serverType@ is used for defining the 'Servant.Docs.HasDocs'
-- instance. There are instances for 'Servant.Client.HasClient' and
-- 'Servant.Server.HasServer' for 'RawM\'' with a polymorphic phantom type, but
-- there is only a 'Servant.Docs.HasDocs' instance specified for @'RawM\''
-- 'FileServer'@. This allows the end-user to easily create a
-- 'Servant.Docs.HasDocs' instance for a custom 'Network.Wai.Application'.
data RawM' serverType deriving Typeable

-- | Used by 'RawM' as a phantom type.
data FileServer deriving Typeable
