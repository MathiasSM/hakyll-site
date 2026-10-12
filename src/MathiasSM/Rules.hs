module MathiasSM.Rules (rules) where

import Hakyll (Rules, compile, match, templateBodyCompiler)
import MathiasSM.Config (SiteConfig, projectsGroup)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.Feed (processFeeds)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcaseGroup)
import MathiasSM.Rules.SinglePages (page, processPage)
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

-- | Processes a group: posts for most groups, projects for the showcase
processGroup :: SiteConfig -> String -> Rules ()
processGroup cfg group
    | group == projectsGroup = processShowcaseGroup cfg
    | otherwise = processPostGroup cfg group
