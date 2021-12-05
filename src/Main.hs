--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.List (find, isSuffixOf)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid (mappend)
import Hakyll
import MathiasSM.CleanURL
import MathiasSM.Compile
import MathiasSM.Context
import MathiasSM.Metadata
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (process404, processContactPage, processSinglePage, processTestPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import Text.Pandoc.Options (HTMLMathMethod (MathJax), writerHTMLMathMethod)

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
