-- | The site's rules: how every source file is compiled, routed, and combined
module Cozyll.Rules (rules) where

import Control.Monad (when)
import Cozyll.CleanURL (pathRoute)
import Cozyll.Config (Env (Dev), SiteConfig, projectsGroup, siteEnv, templatePattern)
import Cozyll.Discover (discoverGroups, discoverStandalonePages)
import Cozyll.Domain.Website (Website)
import Cozyll.Rules.Assets (processAssets)
import Cozyll.Rules.Feed (processFeeds)
import Cozyll.Rules.PostGroup (processPostGroup)
import Cozyll.Rules.Redirects (processRedirects)
import Cozyll.Rules.Showcase (processShowcaseGroup)
import Cozyll.Rules.SinglePages (page, pageCompiler, processPage)
import Cozyll.Rules.Sitemap (processSitemap)
import Cozyll.Rules.Validate (validateNavEntries)
import Cozyll.Rules.WellKnown (processWellKnownFiles)
import Cozyll.Theme (rewritingTemplateCompiler)
import Hakyll (Rules, compile, match, metadataRoute, route)

-- | Assembles the whole site from the content and configuration
rules :: SiteConfig -> Website -> Rules ()
rules cfg website = do
    match (templatePattern cfg) $ compile (rewritingTemplateCompiler cfg)
    validateNavEntries cfg
    processAssets cfg
    processWellKnownFiles cfg website
    standalone <- discoverStandalonePages cfg
    mapM_ (processPage cfg . page cfg) standalone
    groups <- discoverGroups cfg
    mapM_ (processGroup cfg) groups
    processFeeds cfg website
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
