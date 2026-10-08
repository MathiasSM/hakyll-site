-- | The site's Hakyll rules, shared by the @site@ executable and the snapshot test
module MathiasSM.Site (rules) where

import Hakyll (Rules, compile, match, templateBodyCompiler)
import MathiasSM.Config (postGroups, showcaseName, standalonePages)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.Feed (processFeeds)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (page, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Validate (validateHobbyPages, validateNavEntries)

rules :: Rules ()
rules = do
  match "templates/*" $ compile templateBodyCompiler
  validateHobbyPages
  validateNavEntries
  processAssets
  mapM_ (processPage . page) (standalonePages <> ["_test"])
  processShowcase showcaseName
  mapM_ processPostGroup postGroups
  processFeeds
  processSitemap
  processRedirects
