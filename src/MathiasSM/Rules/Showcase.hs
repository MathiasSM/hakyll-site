{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.Showcase (processShowcase) where

import Data.String (fromString)
import Hakyll
    ( Rules,
      Metadata,
      Context,
      Compiler,
      getResourceString,
      saveSnapshot,
      loadAllSnapshots,
      composeRoutes,
      constRoute,
      compile,
      match,
      matchMetadata,
      route,
      listField,
      loadAndApplyTemplate )
import MathiasSM.CleanURL ( cleanRoute )
import MathiasSM.Compile ( runPandoc, finish )
import MathiasSM.Context ( minimalCtx, navStateContext )

-- | Processes a group: its index page and all the item pages
processShowcase :: String -> Rules ()
processShowcase name = do
  processShowcaseItems name
  processShowcaseIndex name

-- | Builds the group index page as an archive page
processShowcaseIndex :: String -> Rules ()
processShowcaseIndex pageName = match pagePattern $ do
  route $ constRoute pageRoute `composeRoutes` cleanRoute
  compile $ do
    ctx <- getCtx pageName
    getResourceString
      >>= runPandoc
      >>= loadAndApplyTemplate "templates/minimal.html" ctx
      >>= loadAndApplyTemplate "templates/with-projects.html" ctx
      >>= loadAndApplyTemplate "templates/as-page.html" ctx
      >>= finish (navStateContext pageName <> minimalCtx)
 where
  pagePattern = fromString $ "data/pages/" ++ pageName ++ ".md"
  pageRoute = pageName

-- | Builds each item/post page
processShowcaseItems :: String -> Rules ()
processShowcaseItems name =
  let groupItemsPattern = fromString "data/projects/**"
      groupSnapshot = fromString $ "published-" ++ name
   in matchMetadata groupItemsPattern hasMinimalMetadata $
        compile $
          getResourceString >>= runPandoc >>= saveSnapshot groupSnapshot

-- | Checks if item has all needed metadata
-- |
-- | Should be  something like:
-- | all (\f -> f m) [hasTitle, hasHref, hasStatus, hasStartDate, hasDescription]
hasMinimalMetadata :: Metadata -> Bool
hasMinimalMetadata _ = True

-- | Build context for archive page
getCtx :: String -> Compiler (Context String)
getCtx groupName =
  let groupItemsPattern = fromString "data/projects/**"
      groupSnapshot = fromString $ "published-" ++ groupName
   in do
        posts <- loadAllSnapshots groupItemsPattern groupSnapshot
        return $ listField "projects" minimalCtx (return posts) <> minimalCtx
