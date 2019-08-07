{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mappend)
import Hakyll
import Control.Monad
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  hakyll $ do
    match "img/**" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "js/*" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/page.html" postCtx >>=
        loadAndApplyTemplate "templates/wrapper.html" defaultContext >>=
        relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
    match "starred.md" $ compile pandocCompiler
    match "about.md" $ version "index" $ do
      route $ constRoute "index.html"
      compile $ do
        posts <- loadAll "posts/*"
        starred <- itemBody <$> load "starred.md" :: Compiler String
        let pinned = filterCategory "pinned" =<< recentFirst posts
            recent = take 5 <$> recentFirst posts
        makeItem "" >>=
          loadAndApplyTemplate
            "templates/index.html"
            ( wList "recent" recent
            . wList "pinned" pinned 
            . wConst "starred" starred
            $ defaultContext ) >>=
          loadAndApplyTemplate
            "templates/wrapper.html"
            (wConst "title" "Adithya Kumar" defaultContext) >>=
          relativizeUrls
    match "about.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate
          "templates/page.html"
          defaultContext >>=
        loadAndApplyTemplate
          "templates/wrapper.html"
          defaultContext >>=
        relativizeUrls

postCtx :: Context String
postCtx = wDate . wDef $ constField "author" "Adithya Kumar"

wDate :: Context String -> Context String
wDate = mappend (dateField "date" "%B %e, %Y")

wConst :: String -> String -> Context String -> Context String
wConst i x = mappend (constField i x)

wDef :: Context String -> Context String
wDef = mappend defaultContext

wList :: String -> Compiler [Item String] -> Context String -> Context String
wList x cx = mappend (listField x postCtx cx)

filterCategory :: String -> [Item String] -> Compiler [Item String]
filterCategory x = filterM f
  where
    f = fmap (elem x . words . fromMaybe "") . flip getMetadataField "category" . itemIdentifier




