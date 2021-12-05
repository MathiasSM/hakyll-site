{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.Showcase (processShowcase) where

import Data.List (sortOn)
import Data.String (fromString)
import Hakyll
import MathiasSM.CleanURL
import MathiasSM.Compile
import MathiasSM.Context
import MathiasSM.Metadata

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
    getResourceString >>= runPandoc
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
hasMinimalMetadata :: Metadata -> Bool
hasMinimalMetadata m = True --all (\f -> f m) [hasTitle, hasHref, hasStatus, hasStartDate, hasDescriptions]

-- | Build context for archive page
getCtx :: String -> Compiler (Context String)
getCtx groupName =
  let groupItemsPattern = fromString "data/projects/**"
      groupSnapshot = fromString $ "published-" ++ groupName
   in do
        posts <- loadAllSnapshots groupItemsPattern groupSnapshot
        return $ listField "projects" minimalCtx (return posts) <> minimalCtx
