module MathiasSM.Rules.Assets (processAssets) where

import Data.String (fromString)
import Hakyll (
  Item (itemBody),
  Pattern,
  Routes,
  Rules,
  applyAsTemplate,
  compile,
  complement,
  composeRoutes,
  compressCssCompiler,
  copyFileCompiler,
  create,
  getResourceLBS,
  getResourceString,
  gsubRoute,
  idRoute,
  loadAll,
  makeItem,
  match,
  route,
  setExtension,
  templateBodyCompiler,
  unixFilterLBS,
  version,
  (.&&.),
  (.||.),
 )
import MathiasSM.Config (assetsDir, tablesPattern)
import MathiasSM.Context.Site (siteContext)
import MathiasSM.Rules.Favicon (faviconRules)
import System.FilePath.Posix ((</>))

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
This includes those in `assets/static/`, and final versions of `assets/images/`
Should NOT include SVGs, and unprocessed images (.dot, etc)
-}
processStaticFiles :: Rules ()
processStaticFiles = do
  justCopy (imagesPattern ["jpg", "png", "gif"]) assetRoute
  justCopy "favicon.ico" idRoute
  justCopy (staticPattern .&&. complement templatedStatic) staticRoute
  processTemplatedStatic

-- | Makes tables (TSV) loadable by contexts and rules; they are not routed
processTables :: Rules ()
processTables = match tablesPattern $ compile getResourceString

-- | Compress all CSS as one file
processCss :: Rules ()
processCss = do
  match cssFiles $ compile compressCssCompiler
  create ["styles.css"] $ do
    route idRoute
    compile $ do
      css <- loadAll cssFiles
      makeItem $ unlines $ map itemBody css

-- | Uses unix external filter (dot) to compile them as png
processDotImages :: Rules ()
processDotImages = match (imagesPattern ["dot"]) $ do
  route $ assetRoute `composeRoutes` setExtension "png"
  compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

-- | Compiles SVG into context, usable by templates to include directly in HTML, and copies it as an image
processSvgImages :: Rules ()
processSvgImages = do
  -- Allows including directly in html
  match (imagesPattern ["svg"]) $ compile templateBodyCompiler
  -- Allows importing as img with src="<...>.svg"
  match (imagesPattern ["svg"]) $
    version "svg" $ do
      route $ assetRoute `composeRoutes` setExtension "svg"
      compile copyFileCompiler

-- | Generates the favicons from the logo
processFavicon :: Rules ()
processFavicon = do
  faviconRules $ fromString $ assetsDir </> "images" </> "logo.svg"

{- | Static files that mention the site's address or name; they are filled in from
the site context (`$site-baseUrl$`, `$site-domain$`, ...) instead of copied as is
-}
templatedStatic :: Pattern
templatedStatic =
  foldr1
    (.||.)
    [ staticFile name
    | name <-
        [ "CNAME"
        , "robots.txt"
        , "funding.json"
        , "manifest.webmanifest"
        , ".well-known/security.txt"
        , ".well-known/funding-manifest-urls"
        ]
    ]

processTemplatedStatic :: Rules ()
processTemplatedStatic = match templatedStatic $
  version "raw" $ do
    route staticRoute
    compile $ getResourceString >>= applyAsTemplate siteContext

-- | Rule to copy static files
justCopy :: Pattern -> Routes -> Rules ()
justCopy something routes = match something $
  version "raw" $ do
    route routes
    compile copyFileCompiler

-- | Where the site's own files live (see 'assetsDir')
cssFiles, staticPattern :: Pattern
cssFiles = fromString $ assetsDir </> "css" </> "*.css"
staticPattern = fromString $ assetsDir </> "static" </> "**"

-- | A file under `assets/static/`
staticFile :: FilePath -> Pattern
staticFile name = fromString $ assetsDir </> "static" </> name

-- | Files with one of the extensions, anywhere under `assets/images/`
imagesPattern :: [String] -> Pattern
imagesPattern extensions = foldr1 (.||.) [fromString $ assetsDir </> "images" </> ("**." ++ ext) | ext <- extensions]

-- | Publishes `assets/images/x` as `images/x`
assetRoute :: Routes
assetRoute = gsubRoute ("^" ++ assetsDir ++ "/") (const "")

-- | Publishes `assets/static/x` as `x`, at the site root
staticRoute :: Routes
staticRoute = gsubRoute ("^" ++ assetsDir ++ "/static/") (const "")
