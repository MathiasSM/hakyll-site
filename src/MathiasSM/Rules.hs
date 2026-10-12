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
import MathiasSM.Validate (validateHobbyPages, validateNavEntries)

rules :: SiteConfig -> Rules ()
rules cfg = do
    match "templates/*" $ compile templateBodyCompiler
    validateHobbyPages cfg
    validateNavEntries cfg
    processAssets cfg
    mapM_ (processPage cfg . page cfg) (standalonePages <> ["_test"])
    processShowcase cfg showcaseName
    mapM_ (processPostGroup cfg) postGroups
    processFeeds cfg
    processSitemap cfg
    processRedirects cfg
