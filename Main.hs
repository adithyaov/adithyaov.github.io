{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main =
  hakyll $ do
    match "img/*" $ do
      route idRoute
      compile copyFileCompiler
    match "js/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "vendor/**" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate
          "templates/post.html"
          (wConst "base" ".." . wDate . wDef $ constField "author" "Adithya Kumar") >>=
        loadAndApplyTemplate
          "templates/wrapper.html"
          (wConst "year" "2019" . wConst "base" ".." $ defaultContext) >>=
        relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
    match "templates/index.html" $ version "index" $ do
      route $ constRoute "index.html"
      compile $
        loadAll "posts/*" >>= \posts ->
          getResourceBody >>=
          applyAsTemplate
            (wConst "base" "." . wDef $ listField "posts" (wDate . wDef $ constField "author" "Adithya Kumar") (take 5 <$> recentFirst posts)) >>=
          loadAndApplyTemplate
            "templates/wrapper.html"
            (wConst "year" "2019" . wConst "base" "." $ defaultContext) >>=
         relativizeUrls
    match "templates/index.html" $ version "archive" $ do
      route $ constRoute "archive.html"
      compile $
        loadAll "posts/*" >>= \posts ->
          getResourceBody >>=
          applyAsTemplate
            (wConst "archive" "true" . wConst "base" "." . wDef $ listField "posts" (wDate . wDef $ constField "author" "Adithya Kumar") (recentFirst posts)) >>=
          loadAndApplyTemplate
            "templates/wrapper.html"
            (wConst "year" "2019" . wConst "base" "." $ defaultContext) >>=
          relativizeUrls

wDate :: Context String -> Context String
wDate = mappend (dateField "date" "%B %e, %Y")

wConst :: String -> String -> Context String -> Context String
wConst i x = mappend (constField i x)

wDef :: Context String -> Context String
wDef = mappend defaultContext
