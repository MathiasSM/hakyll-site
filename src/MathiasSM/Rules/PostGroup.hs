{-# LANGUAGE OverloadedStrings #-}

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
  let groupIndexPattern = fromString $ "data/pages/" ++ groupName ++ ".md"
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
   in matchMetadata groupItemsPattern hasMinimalMetadata $ do
        route $ metadataRoute getMetadataRoute `composeRoutes` cleanRoute
        compile $
          getResourceString
            >>= runPandoc
            >>= saveSnapshot groupSnapshot
            >>= loadAndApplyTemplate "templates/minimal.html" minimalCtx
            >>= loadAndApplyTemplate "templates/as-post.html" minimalCtx
            >>= finish (postSocialTagsContext <> navStateContext groupName <> minimalCtx)

-- | Checks if item has all needed metadata
hasMinimalMetadata :: Metadata -> Bool
hasMinimalMetadata m = all (\f -> f m) [hasTitle, hasPublishedDate, hasPath]

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
getMetadataRoute m = constRoute (metadataPath m) `composeRoutes` gsubRoute "^/" (const "")
