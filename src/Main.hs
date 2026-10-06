--------------------------------------------------------------------------------
import Hakyll
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (processKnownPage, processKnownPage')
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Trust (processTrust)
import MathiasSM.Context (minimalCtx)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "templates/*" $ compile templateBodyCompiler
  processAssets
  processKnownPage "about" []
  processKnownPage "contact" ["templates/with-social-links.html"]
  processKnownPage' False (return minimalCtx) "_test" []
  processKnownPage' False (return minimalCtx) "404" []
  processShowcase "showcase"
  processPostGroup "blog"
  processPostGroup "escritos"
  processSitemap
  processRedirects
  processTrust
