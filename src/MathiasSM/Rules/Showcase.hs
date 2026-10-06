module MathiasSM.Rules.Showcase (processShowcase) where

import Data.String (fromString)
import Hakyll
    ( Rules,
      Context,
      Compiler,
      getResourceString,
      saveSnapshot,
      loadAllSnapshots,
      compile,
      matchMetadata,
      listField, Snapshot, Pattern)
import MathiasSM.Compile ( runPandoc )
import MathiasSM.Context ( minimalCtx )
import MathiasSM.Rules.SinglePages (processKnownPage')
import MathiasSM.Metadata (projectMetadata)

-- | Processes a group: its index page and all the item pages
processShowcase :: String -> Rules ()
processShowcase name = do
  processShowcaseItems name
  processShowcaseIndex name

-- | Builds the group index page as an archive page
processShowcaseIndex :: String -> Rules ()
processShowcaseIndex pageName = processKnownPage' True (getProjectsCtx pageName) pageName ["templates/with-projects.html"]
  
groupItemsPattern :: Pattern
groupItemsPattern = "data/projects/**"

groupSnapshot :: String -> Snapshot
groupSnapshot groupName = fromString $ "published-" ++ groupName

-- | Builds each item/post page
processShowcaseItems :: String -> Rules ()
processShowcaseItems groupName =
  matchMetadata groupItemsPattern projectMetadata $
        compile $
          getResourceString >>= runPandoc >>= saveSnapshot (groupSnapshot groupName)


-- | Build context for archive page
getProjectsCtx :: String -> Compiler (Context String)
getProjectsCtx groupName = do
  posts <- loadAllSnapshots groupItemsPattern (groupSnapshot groupName)
  return $ listField "projects" minimalCtx (return posts) <> minimalCtx
