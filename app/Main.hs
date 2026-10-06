import Hakyll
import MathiasSM.Config (postGroups, showcaseName)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (Page (..), page, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Trust (processTrust)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "templates/*" $ compile templateBodyCompiler
  processAssets
  mapM_
    processPage
    [ page "about"
    , page "contact"
    , page "_test"
    , page "404"
    ]
  processShowcase showcaseName
  mapM_ processPostGroup postGroups
  processSitemap
  processRedirects
  processTrust
