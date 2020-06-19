{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Proxy   (Proxy (Proxy))
import Data.Text    (Text)
import Servant.Docs (ToSample (toSamples), docs, markdown)

import Servant.RawM.Docs ()

import Api (Api)

instance ToSample Int where
  toSamples :: Proxy Int -> [(Text, Int)]
  toSamples Proxy = [("example Int", 3)]

-- | Print the documentation rendered as markdown to stdout.
main :: IO ()
main = putStrLn . markdown $ docs (Proxy :: Proxy Api)
