{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.RawM.Internal.Docs

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports a 'HasDocs' instance for 'RawM'.
-}

module Servant.RawM.Internal.Docs where

import Control.Lens ((.~), (<>~))
import Data.Function ((&))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Network.HTTP.Media ((//))
import Network.HTTP.Types (methodGet)
import Servant.Docs
       (Action, API, DocCapture(DocCapture), DocNote(DocNote), DocOptions,
        Endpoint, HasDocs(docsFor), captures, defResponse, method, notes,
        path, respBody, respStatus, respTypes, response, single)
-- import Servant.Docs.Internal (apiEndpoints, respBody, response)

import Servant.RawM.Internal.API (FileServer, RawM')

instance (HasDocs serverType) => HasDocs (RawM' serverType) where
  docsFor :: Proxy (RawM' serverType) -> (Endpoint, Action) -> DocOptions -> API
  docsFor Proxy (endpoint, action) docOpts =
    docsFor (Proxy :: Proxy serverType) (endpoint, action) docOpts

instance HasDocs FileServer where
  docsFor :: Proxy FileServer -> (Endpoint, Action) -> DocOptions -> API
  docsFor Proxy (endpoint, action) _ =
    let captureSymbol = ":filename"
        captureDesc = "the name of the file to GET"
        capture = DocCapture captureSymbol captureDesc
        htmlMediaType = "text" // "html"
        jsMediaType = "application" // "javascript"
        possibleRespMediaTypes =
          [ jsMediaType
          , "application" // "xml"
          , "application" // "zip"
          , "application" // "pdf"
          , "audio" // "mpeg"
          , "text" // "css"
          , htmlMediaType
          , "text" // "csv"
          , "text" // "plain"
          , "image" // "png"
          , "image" // "jpeg"
          , "image" // "gif"
          ]
        sampleHtml = "<html><body><p>Hello World</p></body></html>"
        sampleHtmlResp = ("example HTML response", htmlMediaType, sampleHtml)
        sampleJs = "alert(\"Hello World\");"
        sampleJsResp = ("example JS response", jsMediaType, sampleJs)
        noteBody1 =
          "This route is a file server.  If you specify the filename, it " <>
          "will serve up the file with the appropriate content type.  If " <>
          "the file doesn't exist, it will return a 404."
        noteBody2 =
          "The supported content types below are just examples.  A full " <>
          "list of recognized MIME types can be found " <>
          "[here](https://hackage.haskell.org/package/mime-types-0.1.0.7/docs/src/Network.Mime.html#defaultMimeMap)."
        note = DocNote "File Server" [noteBody1, noteBody2]
        resp =
          defResponse
            & respStatus .~ 200
            & respTypes .~ possibleRespMediaTypes
            & respBody .~ [sampleHtmlResp, sampleJsResp]
        newEndpoint =
          endpoint
            & method .~ methodGet
            & path <>~ [captureSymbol]
        newAction =
          action
            & captures <>~ [capture]
            & notes <>~ [note]
            & response .~ resp
    in single newEndpoint newAction
