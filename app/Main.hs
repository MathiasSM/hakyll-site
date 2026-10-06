import Hakyll
import MathiasSM.Config (postGroups, showcaseName, standalonePages)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (page, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Trust (processTrust)
import MathiasSM.Validate (ensureContentExists)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  ensureContentExists
  hakyll rules

rules :: Rules ()
rules = do
  match "templates/*" $ compile templateBodyCompiler
  processAssets
  mapM_ (processPage . page) (standalonePages <> ["_test"])
  processShowcase showcaseName
  mapM_ processPostGroup postGroups
  processSitemap
  processRedirects
  processTrust
