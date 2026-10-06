module MathiasSM.Rules.PostGroup (processPostGroup) where

import Data.String (fromString)
import Hakyll
import MathiasSM.CleanURL
import MathiasSM.Compile
import MathiasSM.Context
import MathiasSM.Metadata

-- | Processes a group: its index page and all the item pages
processPostGroup :: String -> Rules ()
processPostGroup groupName = do
  processPostGroupIndex groupName
  processPostGroupItems groupName

-- | Builds the group index page as an archive page
processPostGroupIndex :: String -> Rules ()
processPostGroupIndex groupName =
  let groupIndexPattern = fromString $ "data/pages/" ++ groupName ++ ".*"
   in match groupIndexPattern $ do
        route $ constRoute groupName `composeRoutes` cleanRoute
        compile $ do
          ctx <- getCtx groupName
          getResourceString
            >>= runPandoc
            >>= loadAndApplyTemplate "templates/minimal.html" ctx
            >>= loadAndApplyTemplate "templates/with-posts.html" ctx
            >>= loadAndApplyTemplate "templates/as-page.html" ctx
            >>= finish (navStateContext groupName <> minimalCtx)

-- | Builds each item/post page
processPostGroupItems :: String -> Rules ()
processPostGroupItems groupName =
  let groupItemsPattern = fromString $ "data/posts/" ++ groupName ++ "/**"
      groupSnapshot = fromString $ "published-" ++ groupName
   in matchMetadata groupItemsPattern postMetadata $ do
        route $ metadataRoute getMetadataRoute `composeRoutes` cleanRoute
        compile $
          getResourceString
            >>= runPandoc
            >>= saveSnapshot groupSnapshot
            >>= loadAndApplyTemplate "templates/minimal.html" minimalCtx
            >>= loadAndApplyTemplate "templates/as-post.html" minimalCtx
            >>= finish (postSocialTagsContext <> navStateContext groupName <> minimalCtx)

-- | Build context for archive page
getCtx :: String -> Compiler (Context String)
getCtx groupName =
  let groupItemsPattern = fromString $ "data/posts/" ++ groupName ++ "/**"
      groupSnapshot = fromString $ "published-" ++ groupName
   in do
        posts <- recentFirst =<< loadAllSnapshots groupItemsPattern groupSnapshot
        return $ listField "posts" minimalCtx (return posts) <> minimalCtx

-- | Gets route from metadata "path" field
getMetadataRoute :: Metadata -> Routes
getMetadataRoute m = case lookupKey Path m of
  Just path -> constRoute path `composeRoutes` gsubRoute "^/" (const "")
  Nothing -> customRoute $ \ident -> error $ "Missing `path` in " ++ toFilePath ident
