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
import MathiasSM.Config (SiteConfig, minimalTemplate, postSnapshot, postTemplate, postsPattern)
import MathiasSM.Content (isDraft, requirePost)
import MathiasSM.Context (minimalCtx, postSocialTagsContext)
import MathiasSM.Context.Nav (navContext)
import MathiasSM.Rules.SinglePages (Page (..), page, processPage)

-- | Processes a group: its index page and all the item pages
processPostGroup :: SiteConfig -> String -> Rules ()
processPostGroup cfg groupName = do
    processPostGroupIndex cfg groupName
    processPostGroupItems cfg groupName

-- | Builds the group index page as an archive page
processPostGroupIndex :: SiteConfig -> String -> Rules ()
processPostGroupIndex cfg groupName =
    processPage cfg (page cfg groupName){pageContext = getCtx cfg groupName, pageTemplates = ["templates/with-posts.html"]}

{- | Builds each item/post page

Anything but a draft must have valid front matter, or the build fails.
-}
processPostGroupItems :: SiteConfig -> String -> Rules ()
processPostGroupItems cfg groupName =
    matchMetadata (postsPattern cfg groupName) (not . isDraft) $ do
        route $ metadataRoute pathRoute
        compile $
            requirePost
                >> getResourceString
                >>= runPandoc
                >>= saveSnapshot (postSnapshot groupName)
                >>= applyTemplates (minimalCtx cfg) [minimalTemplate, postTemplate]
                >>= finish (postSocialTagsContext cfg <> navContext cfg groupName <> minimalCtx cfg)

-- | Build context for archive page
getCtx :: SiteConfig -> String -> Compiler (Context String)
getCtx cfg groupName = do
    posts <- recentFirst =<< loadAllSnapshots (postsPattern cfg groupName) (postSnapshot groupName)
    return $ listField "posts" (minimalCtx cfg) (return posts) <> minimalCtx cfg
