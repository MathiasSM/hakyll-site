{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.SinglePages (processSinglePage, processContactPage, process404, processTestPage) where

import Data.String (fromString)
import Hakyll
import MathiasSM.CleanURL
import MathiasSM.Compile
import MathiasSM.Context
import MathiasSM.Metadata

-- | Configures which page is to be HOME (and placed in /index.html)
homePageName :: String
homePageName = "about"

-- | Processes a given standalone page
-- If pageName is that of homePageName, page is placed under /index.html path
processSinglePage :: String -> Rules ()
processSinglePage pageName = match pagePattern $ do
  route $ constRoute pageRoute `composeRoutes` cleanRoute
  compile $
    getResourceString >>= runPandoc
      >>= loadAndApplyTemplate "templates/minimal.html" minimalCtx
      >>= loadAndApplyTemplate "templates/as-page.html" minimalCtx
      >>= finish (navStateContext pageName <> minimalCtx)
  where
    pagePattern = fromString $ "data/pages/" ++ pageName ++ ".md"
    pageRoute = finalRoute pageName
    finalRoute x
      | x == homePageName = ""
      | otherwise = pageName

-- pageRoute pageName = metadataRoute
--   where
--     getMetadataHome m = constRoute (metadataHome m) `composeRoutes` gsubRoute "^/" (const "")

-- | Processes a given standalone page
-- If pageName is that of homePageName, page is placed under /index.html path
processContactPage :: String -> Rules ()
processContactPage pageName = match pagePattern $ do
  route $ constRoute pageRoute `composeRoutes` cleanRoute
  compile $
    getResourceString >>= runPandoc
      >>= loadAndApplyTemplate "templates/minimal.html" minimalCtx
      >>= loadAndApplyTemplate "templates/with-social-links.html" minimalCtx
      >>= loadAndApplyTemplate "templates/as-page.html" minimalCtx
      >>= finish (navStateContext pageName <> minimalCtx)
  where
    pagePattern = fromString $ "data/pages/" ++ pageName ++ ".md"
    pageRoute = finalRoute pageName
    finalRoute x
      | x == homePageName = ""
      | otherwise = pageName

-- | Creates a 404.html used by github pages on 404 errors
process404 :: Rules ()
process404 = match "data/pages/404.md" $ do
  route $ constRoute "404.html" -- Do NOT make clean
  compile $
    getResourceString >>= runPandoc
      >>= loadAndApplyTemplate "templates/minimal.html" minimalCtx
      >>= loadAndApplyTemplate "templates/as-page.html" minimalCtx
      >>= finish minimalCtx

-- | Not to be used in production, creates a test page from markdown
-- TODO: Ensure it's not used in production
processTestPage :: Rules ()
processTestPage = match "data/pages/_test.md" $ do
  route $ constRoute "_test.html" -- Do NOT make clean
  compile $
    getResourceString >>= runPandoc
      >>= loadAndApplyTemplate "templates/minimal.html" minimalCtx
      >>= loadAndApplyTemplate "templates/as-page.html" minimalCtx
      >>= finish minimalCtx
