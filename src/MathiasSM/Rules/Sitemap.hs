module MathiasSM.Rules.Sitemap (processSitemap) where

import Control.Monad (filterM)
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
import MathiasSM.CleanURL (cleanIndexHtmls)
import MathiasSM.Config (SiteConfig, pagesPattern, postsPattern)
import MathiasSM.Content (isSitemapExcluded)
import MathiasSM.Context (minimalCtx)
import MathiasSM.Writings (discoverGroups)

-- | Builds sitemap.xml from the standalone pages and every post group
processSitemap :: SiteConfig -> Rules ()
processSitemap cfg = create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
        groups <- discoverGroups cfg
        posts <- mapM (\group -> recentFirst =<< loadAll (postsPattern cfg group)) groups
        pages <- loadAll $ pagesPattern cfg "*"
        let allItems = filterM isIndexable (pages <> concat posts)
            sitemapCtx = listField "entries" (minimalCtx cfg) allItems <> minimalCtx cfg
        makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
            >>= cleanIndexHtmls

-- | Whether an item should appear in the sitemap (not marked `sitemap: false`)
isIndexable :: Item String -> Compiler Bool
isIndexable item = not . isSitemapExcluded <$> getMetadata (itemIdentifier item)
