{- |
Module      :  Servant.RawM.Internal.API

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module defines the 'Throws' and 'Throwing' types.
-}

module Servant.RawM.Internal.API where

import Data.Typeable (Typeable)

data RawM deriving Typeable
