module MathiasSM.Rules.Showcase (processShowcase) where

import Data.List (sortOn)
import Hakyll (
  Compiler,
  Context,
  Item (Item, itemBody, itemIdentifier),
  Rules,
  compile,
  getResourceString,
  listField,
  loadAll,
  match,
 )
import MathiasSM.Config (projectsPattern)
import MathiasSM.Content (requireProject, showcaseKey)
import MathiasSM.Context (minimalCtx)
import MathiasSM.Context.Project (projectContext)
import MathiasSM.Rules.SinglePages (Page (..), page, processPage)

-- | Processes the showcase: its index page and the projects it lists
processShowcase :: String -> Rules ()
processShowcase name = do
  processShowcaseItems
  processShowcaseIndex name

-- | Builds the index page, listing all projects
processShowcaseIndex :: String -> Rules ()
processShowcaseIndex name =
  processPage (page name){pageContext = getProjectsCtx, pageTemplates = ["templates/with-projects.html"]}

{- | Compiles each project (not routed), so the index can list it

The raw YAML is kept as the item; parsing here makes an invalid project fail the build.
-}
processShowcaseItems :: Rules ()
processShowcaseItems =
  match projectsPattern $
    compile $ do
      item <- getResourceString
      _ <- requireProject item
      return item

-- | Build context for the index page
getProjectsCtx :: Compiler (Context String)
getProjectsCtx = do
  items <- loadAll projectsPattern
  projects <- mapM (\item -> Item (itemIdentifier item) <$> requireProject item) items
  let ordered = sortOn (showcaseKey . itemBody) projects
  return $ listField "projects" projectContext (return ordered) <> minimalCtx
