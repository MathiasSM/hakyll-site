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
      getResourceString,
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
import MathiasSM.Config ( tablesPattern )
import MathiasSM.Rules.Favicon ( faviconRules )

-- | Processes all assets (images or otherwise) into final site
processAssets :: Rules ()
processAssets = do
  processCss
  processSvgImages
  processDotImages
  processFavicon
  processStaticFiles
  processTables

{- | Copies files directly to output folder
This includes those in `static/`, and final versions of `images/`
Should NOT include SVGs, and unprocessed images (.dot, etc)
-}
processStaticFiles :: Rules ()
processStaticFiles = do
  justCopy ("images/**.jpg" .||. "images/**.png" .||. "images/**.gif") idRoute
  justCopy "favicon.ico" idRoute
  justCopy "static/**" rootRoute

-- | Makes tables (TSV) loadable by contexts and rules; they are not routed
processTables :: Rules ()
processTables = match tablesPattern $ compile getResourceString

{- | Compress all CSS as one file -}
processCss :: Rules ()
processCss = do
  match "css/*" $ compile compressCssCompiler
  create ["styles.css"] $ do
    route idRoute
    compile $ do
      css <- loadAll "css/*.css"
      makeItem $ unlines $ map itemBody css

-- | Uses unix external filter (dot) to compile them as png
processDotImages :: Rules ()
processDotImages = match "images/**.dot" $ do
  route $ setExtension "png"
  compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

-- | Compiles SVG into context, usable by templates to include directly in HTML, and copies it as an image
processSvgImages :: Rules ()
processSvgImages = do
  -- Allows including directly in html
  match "images/**.svg" $ compile templateBodyCompiler
  -- Allows importing as img with src="<...>.svg"
  match "images/**.svg" $
    version "svg" $ do
      route $ setExtension "svg"
      compile copyFileCompiler

-- | Generates the favicons from the logo
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
rootRoute = gsubRoute "^static/" (const "")
