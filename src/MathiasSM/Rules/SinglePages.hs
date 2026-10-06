module MathiasSM.Rules.SinglePages (Page (..), page, processPage) where

import Hakyll (
  Compiler,
  Context,
  Identifier,
  Rules,
  applyAsTemplate,
  compile,
  getMetadata,
  getResourceString,
  getUnderlying,
  match,
  metadataRoute,
  route,
 )
import MathiasSM.CleanURL (pathRoute)
import MathiasSM.Compile (applyTemplates, finish, runPandoc)
import MathiasSM.Config (minimalTemplate, pagesPattern, pageTemplate)
import MathiasSM.Context (minimalCtx, navStateContext)
import MathiasSM.Metadata (Key (Templated), lookupKey)

-- | A standalone page, backed by content/pages/<pageName>.* and routed by its `path:`
data Page = Page
  { pageName :: String
  , pageContext :: Compiler (Context String)
  -- ^ Context for the page body and its templates
  , pageTemplates :: [Identifier]
  -- ^ Extra templates, applied between the minimal and page templates
  }

-- | A page with the minimal context and no extra templates
page :: String -> Page
page name = Page{pageName = name, pageContext = return minimalCtx, pageTemplates = []}

-- | Processes a standalone page, routed by its `path:` front matter
processPage :: Page -> Rules ()
processPage Page{pageName, pageContext, pageTemplates} = match (pagesPattern pageName) $ do
  route $ metadataRoute pathRoute
  compile $ do
    ctx <- pageContext
    templated <- isTemplated
    getResourceString
      >>= (if templated then applyAsTemplate ctx else return)
      >>= runPandoc
      >>= applyTemplates ctx ([minimalTemplate] <> pageTemplates <> [pageTemplate])
      >>= finish (navStateContext pageName <> ctx)
 where
  -- Pages with `templated: true` may use template syntax in their body
  isTemplated = do
    metadata <- getUnderlying >>= getMetadata
    return $ lookupKey Templated metadata == Just "true"
