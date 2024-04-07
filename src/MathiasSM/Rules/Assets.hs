{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.Assets (processAssets) where

import Hakyll
    ( Pattern,
      Rules,
      Item(itemBody),
      Routes,
      getResourceLBS,
      makeItem,
      loadAll,
      copyFileCompiler,
      (.||.),
      gsubRoute,
      idRoute,
      setExtension,
      compile,
      create,
      match,
      route,
      version,
      unixFilterLBS,
      compressCssCompiler,
      templateBodyCompiler )
import MathiasSM.Rules.Assets.Favicon ( faviconRules )

-- | Processes all assets (images or otherwise) into final site
processAssets :: Rules ()
processAssets = do
  processCss
  processSvgImages
  processDotImages
  processFavicon
  processStaticFiles

{- | Copies files directly to output folder
This includes those in `static/`, and final versions of `images/`
Should NOT include SVGs, and unprocessed images (.dot, etc)
-}
processStaticFiles :: Rules ()
processStaticFiles = do
  justCopy ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif") idRoute
  justCopy "favicon.ico" idRoute
  justCopy "static/**" rootRoute

{- | Compress all CSS as one file
TODO: Check if/when not needed with HTTP2 and caches
-}
processCss :: Rules ()
processCss = do
  match "css/*" $ compile compressCssCompiler
  create ["styles.css"] $ do
    route idRoute
    compile $ do
      css <- loadAll "css/*.css"
      makeItem $ unlines $ map itemBody css

{- | Uses unix external filter (dot) to compile them as png
TODO: Test
TODO: Same for other processed styles
-}
processDotImages :: Rules ()
processDotImages = match "images/*.dot" $ do
  route $ setExtension "png"
  compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

{- | Compiles SVG into context, usable by templates to include directly in HTML
TODO: Figure out how to also output svg version
TODO: Figure out how to also output png versions for icons
-}
processSvgImages :: Rules ()
processSvgImages = do
  -- Allows including directly in html
  match "images/*.svg" $ compile templateBodyCompiler
  -- Allows importing as img with src="<...>.svg"
  match "images/*.svg" $
    version "svg" $ do
      route $ setExtension "svg"
      compile copyFileCompiler

-- Allows importing as img with src="<...>.svg"
processFavicon :: Rules ()
processFavicon = do
  faviconRules "images/logo.svg"

-- | Rule to copy static files
justCopy :: Pattern -> Routes -> Rules ()
justCopy something routes = match something $
  version "raw" $ do
    route routes
    compile copyFileCompiler

-- | Removes `static` prefix from route
rootRoute :: Routes
rootRoute = gsubRoute "^/?static/" (const "")
