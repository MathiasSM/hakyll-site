module MathiasSM.Rules (rules) where

import Control.Monad (when)
import Hakyll (Rules, compile, match, metadataRoute, route, templateBodyCompiler)
import MathiasSM.CleanURL (pathRoute)
import MathiasSM.Config (Env (Dev), SiteConfig, projectsGroup, siteEnv)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.Feed (processFeeds)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcaseGroup)
import MathiasSM.Rules.SinglePages (page, pageCompiler, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Static (processStaticFiles)
import MathiasSM.Validate (validateHobbyPages, validateNavEntries)
import MathiasSM.Website (Website)
import MathiasSM.Writings (discoverGroups, discoverStandalonePages)

rules :: SiteConfig -> Website -> Rules ()
rules cfg website = do
    match "templates/*" $ compile templateBodyCompiler
    validateHobbyPages cfg
    validateNavEntries cfg
    processAssets cfg
    processStaticFiles cfg website
    standalone <- discoverStandalonePages cfg
    mapM_ (processPage cfg . page cfg) standalone
    groups <- discoverGroups cfg
    mapM_ (processGroup cfg) groups
    processFeeds cfg
    processSitemap cfg
    processRedirects cfg
    when (siteEnv cfg == Dev) (processExamplePages cfg)

-- | Processes a group: posts for most groups, projects for the showcase
processGroup :: SiteConfig -> String -> Rules ()
processGroup cfg group
    | group == projectsGroup = processShowcaseGroup cfg
    | otherwise = processPostGroup cfg group

-- | The dev-only preview pages under @dev/example/@, each routed by its own `path:`
processExamplePages :: SiteConfig -> Rules ()
processExamplePages cfg = match "dev/example/**" $ do
    route $ metadataRoute pathRoute
    compile $ pageCompiler cfg (page cfg "example")
