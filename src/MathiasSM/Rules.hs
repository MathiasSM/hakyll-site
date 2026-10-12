module MathiasSM.Rules (rules) where

import Hakyll (Rules, compile, match, templateBodyCompiler)
import MathiasSM.Config (SiteConfig, postGroups, showcaseName, standalonePages)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.Feed (processFeeds)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (page, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Static (processStaticFiles)
import MathiasSM.Validate (validateHobbyPages, validateNavEntries)
import MathiasSM.Website (Website)

rules :: SiteConfig -> Website -> Rules ()
rules cfg website = do
    match "templates/*" $ compile templateBodyCompiler
    validateHobbyPages cfg
    validateNavEntries cfg
    processAssets cfg
    processStaticFiles cfg website
    mapM_ (processPage cfg . page cfg) (standalonePages <> ["_test"])
    processShowcase cfg showcaseName
    mapM_ (processPostGroup cfg) (postGroups cfg)
    processFeeds cfg
    processSitemap cfg
    processRedirects cfg
