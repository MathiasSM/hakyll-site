{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.Sitemap (processSitemap) where

import Data.String (fromString)
import Hakyll
import MathiasSM.CleanURL
import MathiasSM.Compile
import MathiasSM.Context

processSitemap :: Rules ()
processSitemap = create ["sitemap.xml"] $ do
  route idRoute
  compile $ do
    blog <- recentFirst =<< loadAll "data/posts/blog/*"
    escritos <- recentFirst =<< loadAll "data/posts/escritos/*"
    pages <- loadAll "data/pages/*"
    let allItems = return $ pages <> blog <> escritos
        sitemapCtx = listField "entries" minimalCtx allItems <> minimalCtx
    makeItem ""
      >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
      >>= cleanIndexHtmls
