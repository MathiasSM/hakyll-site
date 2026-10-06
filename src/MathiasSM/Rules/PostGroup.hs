module MathiasSM.Rules.PostGroup (processPostGroup) where

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
  metadataRoute,
  recentFirst,
  route,
  saveSnapshot,
 )
import MathiasSM.CleanURL (pathRoute)
import MathiasSM.Compile (applyTemplates, finish, runPandoc)
import MathiasSM.Config (minimalTemplate, postTemplate, postsPattern)
import MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext)
import MathiasSM.Metadata (postMetadata)
import MathiasSM.Rules.SinglePages (Page (..), page, processPage)

-- | Processes a group: its index page and all the item pages
processPostGroup :: String -> Rules ()
processPostGroup groupName = do
  processPostGroupIndex groupName
  processPostGroupItems groupName

-- | Builds the group index page as an archive page
processPostGroupIndex :: String -> Rules ()
processPostGroupIndex groupName =
  processPage (page groupName){pageContext = getCtx groupName, pageTemplates = ["templates/with-posts.html"]}

-- | Builds each item/post page
processPostGroupItems :: String -> Rules ()
processPostGroupItems groupName =
  matchMetadata (postsPattern groupName) postMetadata $ do
    route $ metadataRoute pathRoute
    compile $
      getResourceString
        >>= runPandoc
        >>= saveSnapshot (groupSnapshot groupName)
        >>= applyTemplates minimalCtx [minimalTemplate, postTemplate]
        >>= finish (postSocialTagsContext <> navStateContext groupName <> minimalCtx)

groupSnapshot :: String -> Snapshot
groupSnapshot groupName = "published-" ++ groupName

-- | Build context for archive page
getCtx :: String -> Compiler (Context String)
getCtx groupName = do
  posts <- recentFirst =<< loadAllSnapshots (postsPattern groupName) (groupSnapshot groupName)
  return $ listField "posts" minimalCtx (return posts) <> minimalCtx
