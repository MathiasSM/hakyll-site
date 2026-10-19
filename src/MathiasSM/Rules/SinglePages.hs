-- | Rules for standalone pages
module MathiasSM.Rules.SinglePages (Page (..), page, processPage, pageCompiler) where

import Hakyll (
    Compiler,
    Context,
    Identifier,
    Item,
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
import MathiasSM.Config (SiteConfig, minimalTemplate, pageTemplate, pagesPattern)
import MathiasSM.Context (globalContext)
import MathiasSM.Context.Nav (navContext)
import MathiasSM.Metadata (Key (Templated), lookupKey)

-- | A standalone page, backed by content/writings/<pageName>.* and routed by its `path:`
data Page = Page
    { pageName :: String
    , pageContext :: Compiler (Context String)
    -- ^ Context for the page body and its templates
    , pageTemplates :: [Identifier]
    -- ^ Extra templates, applied between the minimal and page templates
    }

-- | A page with the minimal context and no extra templates
page :: SiteConfig -> String -> Page
page cfg name = Page{pageName = name, pageContext = return (globalContext cfg), pageTemplates = []}

-- | Compiles a standalone page's body (shared by 'processPage' and the dev example pages)
pageCompiler :: SiteConfig -> Page -> Compiler (Item String)
pageCompiler cfg Page{pageName, pageContext, pageTemplates} = do
    ctx <- pageContext
    templated <- isTemplated
    getResourceString
        >>= (if templated then applyAsTemplate ctx else return)
        >>= runPandoc
        >>= applyTemplates ctx ([minimalTemplate] <> pageTemplates <> [pageTemplate])
        >>= finish (navContext cfg pageName <> ctx)
  where
    -- Pages with `templated: true` may use template syntax in their body
    isTemplated = do
        metadata <- getUnderlying >>= getMetadata
        return $ lookupKey Templated metadata == Just "true"

-- | Processes a standalone page, routed by its `path:` front matter
processPage :: SiteConfig -> Page -> Rules ()
processPage cfg page'@Page{pageName} = match (pagesPattern cfg pageName) $ do
    route $ metadataRoute pathRoute
    compile $ pageCompiler cfg page'
