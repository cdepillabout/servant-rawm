{- |
Module      :  Servant.RawM.Internal.API

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)

-}

module Servant.RawM.Internal.API where

import Data.Typeable (Typeable)

type RawM = RawM' FileServer

data RawM' serverType deriving Typeable

data FileServer deriving Typeable
