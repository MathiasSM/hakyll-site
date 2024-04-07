--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (process404, processContactPage, processSinglePage, processTestPage)
import MathiasSM.Rules.Sitemap (processSitemap)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "templates/*" $ compile templateBodyCompiler
  processAssets
  processSinglePage "about" -- Places it under /index.html
  processContactPage "contact"
  processShowcase "showcase"
  processPostGroup "blog"
  processPostGroup "escritos"
  processTestPage -- TODO: Only do on local
  process404
  processSitemap
  processRedirects
