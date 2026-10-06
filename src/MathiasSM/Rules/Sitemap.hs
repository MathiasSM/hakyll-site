module MathiasSM.Rules.Sitemap (processSitemap) where

import Hakyll
    ( Rules,
      makeItem,
      loadAll,
      idRoute,
      compile,
      create,
      route,
      listField,
      loadAndApplyTemplate,
      recentFirst )
import MathiasSM.CleanURL ( cleanIndexHtmls )
import MathiasSM.Config ( pagesPattern, postGroups, postsPattern )
import MathiasSM.Context ( minimalCtx )

-- | Builds sitemap.xml from the standalone pages and every post group
processSitemap :: Rules ()
processSitemap = create ["sitemap.xml"] $ do
  route idRoute
  compile $ do
    posts <- mapM (\group -> recentFirst =<< loadAll (postsPattern group)) postGroups
    pages <- loadAll $ pagesPattern "*"
    let allItems = return $ pages <> concat posts
        sitemapCtx = listField "entries" minimalCtx allItems <> minimalCtx
    makeItem ""
      >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
      >>= cleanIndexHtmls
