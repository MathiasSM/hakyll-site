module MathiasSM.Rules.Showcase (processShowcase) where

import Hakyll (
  Compiler,
  Context,
  Rules,
  Snapshot,
  compile,
  getResourceString,
  listField,
  loadAllSnapshots,
  matchMetadata,
  saveSnapshot,
 )
import MathiasSM.Compile (runPandoc)
import MathiasSM.Config (projectsPattern)
import MathiasSM.Context (minimalCtx)
import MathiasSM.Metadata (projectMetadata)
import MathiasSM.Rules.SinglePages (Page (..), page, processPage)

-- | Processes the showcase: its index page and the project items it lists
processShowcase :: String -> Rules ()
processShowcase name = do
  processShowcaseItems name
  processShowcaseIndex name

-- | Builds the index page, listing all projects
processShowcaseIndex :: String -> Rules ()
processShowcaseIndex name =
  processPage (page name){pageContext = getProjectsCtx name, pageTemplates = ["templates/with-projects.html"]}

groupSnapshot :: String -> Snapshot
groupSnapshot groupName = "published-" ++ groupName

-- | Compiles each project (not routed), so the index can list it
processShowcaseItems :: String -> Rules ()
processShowcaseItems groupName =
  matchMetadata projectsPattern projectMetadata $
    compile $
      getResourceString >>= runPandoc >>= saveSnapshot (groupSnapshot groupName)

-- | Build context for the index page
getProjectsCtx :: String -> Compiler (Context String)
getProjectsCtx groupName = do
  projects <- loadAllSnapshots projectsPattern (groupSnapshot groupName)
  return $ listField "projects" minimalCtx (return projects) <> minimalCtx
