module MathiasSM.Rules.Assets (processAssets) where

import Data.String (fromString)
import Hakyll (
    Item (itemBody),
    Pattern,
    Routes,
    Rules,
    compile,
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
    (.||.),
 )
import MathiasSM.Config (SiteConfig, assetsDir, tablesPattern)
import MathiasSM.Rules.Favicon (faviconRules)
import System.FilePath.Posix ((</>))

-- | Processes all assets (images or otherwise) into final site
processAssets :: SiteConfig -> Rules ()
processAssets cfg = do
    processCss
    processSvgImages
    processDotImages
    processFavicon
    justCopy (imagesPattern ["jpg", "png", "gif"]) assetRoute
    justCopy "favicon.ico" idRoute
    processTables cfg

-- | Makes tables (TSV) loadable by contexts and rules; they are not routed
processTables :: SiteConfig -> Rules ()
processTables cfg = match (tablesPattern cfg) $ compile getResourceString

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

-- | Rule to copy static files
justCopy :: Pattern -> Routes -> Rules ()
justCopy something routes = match something $
    version "raw" $ do
        route routes
        compile copyFileCompiler

-- | Where the site's own files live (see 'assetsDir')
cssFiles :: Pattern
cssFiles = fromString $ assetsDir </> "css" </> "*.css"

-- | Files with one of the extensions, anywhere under `assets/images/`
imagesPattern :: [String] -> Pattern
imagesPattern extensions = foldr1 (.||.) [fromString $ assetsDir </> "images" </> ("**." ++ ext) | ext <- extensions]

-- | Publishes `assets/images/x` as `images/x`
assetRoute :: Routes
assetRoute = gsubRoute ("^" ++ assetsDir ++ "/") (const "")
