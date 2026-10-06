import Control.Monad (unless)
import Hakyll
import MathiasSM.Config (postGroups, showcaseName, siteConfiguration, standalonePages)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (page, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Trust (processTrust)
import MathiasSM.Validate (ensureContentExists)
import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  -- `clean` only removes generated files, so it works without the content
  unless ("clean" `elem` args) ensureContentExists
  hakyllWith siteConfiguration rules

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
