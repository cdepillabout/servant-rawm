{-# LANGUAGE CPP                       #-}

module Main (main) where

import Prelude

-- Semigroup is in Prelude since 4.11.0
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options = doctest $ options <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [
    --   "-XConstraintKinds"
    -- , "-XDataKinds"
      "-XDeriveDataTypeable"
    , "-XDeriveGeneric"
    -- , "-XEmptyDataDecls"
    , "-XFlexibleContexts"
    -- , "-XFlexibleInstances"
    -- , "-XGADTs"
    -- , "-XGeneralizedNewtypeDeriving"
    -- , "-XInstanceSigs"
    -- , "-XMultiParamTypeClasses"
    -- , "-XNoImplicitPrelude"
    , "-XOverloadedStrings"
    -- , "-XPolyKinds"
    -- , "-XRankNTypes"
    -- , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    -- , "-XStandaloneDeriving"
    -- , "-XTupleSections"
    -- , "-XTypeFamilies"
    -- , "-XTypeOperators"
    ]
