module MathiasSM.Rules.Sitemap (processSitemap) where

import Hakyll (
    Rules,
    compile,
    create,
    idRoute,
    listField,
    loadAll,
    loadAndApplyTemplate,
    makeItem,
    recentFirst,
    route,
 )
import MathiasSM.CleanURL (cleanIndexHtmls)
import MathiasSM.Config (SiteConfig, pagesPattern, postsPattern)
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
        let allItems = return $ pages <> concat posts
            sitemapCtx = listField "entries" (minimalCtx cfg) allItems <> minimalCtx cfg
        makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
            >>= cleanIndexHtmls
