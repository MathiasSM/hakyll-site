-- | Rules that build the sitemap
module Cozyll.Rules.Sitemap (processSitemap) where

import Control.Monad (filterM)
import Cozyll.CleanURL (cleanIndexHtmls)
import Cozyll.Config (SiteConfig, pagesPattern, postsPattern, resolveTemplate)
import Cozyll.Content (isSitemapExcluded)
import Cozyll.Context (globalContext)
import Cozyll.Discover (discoverGroups)
import Hakyll (
    Compiler,
    Item,
    Rules,
    compile,
    create,
    getMetadata,
    idRoute,
    itemIdentifier,
    listField,
    loadAll,
    loadAndApplyTemplate,
    makeItem,
    recentFirst,
    route,
 )

-- | Builds sitemap.xml from the standalone pages and every post group
processSitemap :: SiteConfig -> Rules ()
processSitemap cfg = create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
        groups <- discoverGroups cfg
        posts <- mapM (\group -> recentFirst =<< loadAll (postsPattern cfg group)) groups
        pages <- loadAll $ pagesPattern cfg "*"
        let allItems = filterM isIndexable (pages <> concat posts)
            sitemapCtx = listField "entries" (globalContext cfg) allItems <> globalContext cfg
        makeItem ""
            >>= loadAndApplyTemplate (resolveTemplate cfg "sitemap.xml") sitemapCtx
            >>= cleanIndexHtmls

-- | Whether an item should appear in the sitemap (not marked `sitemap: false`)
isIndexable :: Item String -> Compiler Bool
isIndexable item = not . isSitemapExcluded <$> getMetadata (itemIdentifier item)
