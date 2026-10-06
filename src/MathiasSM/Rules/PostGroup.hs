module MathiasSM.Rules.PostGroup (processPostGroup) where

import Hakyll (
  Compiler,
  Context,
  Rules,
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
import MathiasSM.Config (minimalTemplate, postSnapshot, postTemplate, postsPattern)
import MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext)
import MathiasSM.Content (isDraft, requirePost)
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

{- | Builds each item/post page

Anything but a draft must have valid front matter, or the build fails.
-}
processPostGroupItems :: String -> Rules ()
processPostGroupItems groupName =
  matchMetadata (postsPattern groupName) (not . isDraft) $ do
    route $ metadataRoute pathRoute
    compile $
      requirePost
        >> getResourceString
        >>= runPandoc
        >>= saveSnapshot (postSnapshot groupName)
        >>= applyTemplates minimalCtx [minimalTemplate, postTemplate]
        >>= finish (postSocialTagsContext <> navStateContext groupName <> minimalCtx)

-- | Build context for archive page
getCtx :: String -> Compiler (Context String)
getCtx groupName = do
  posts <- recentFirst =<< loadAllSnapshots (postsPattern groupName) (postSnapshot groupName)
  return $ listField "posts" minimalCtx (return posts) <> minimalCtx
