-- | Rules for the assets: styles, media, and tables
module MathiasSM.Rules.Assets (processAssets) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
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
    customRoute,
    getResourceLBS,
    getResourceString,
    idRoute,
    loadAll,
    makeItem,
    match,
    route,
    setExtension,
    templateBodyCompiler,
    toFilePath,
    unixFilterLBS,
    version,
    (.||.),
 )
import MathiasSM.Config (SiteConfig, assetsDir, experiencePattern, hobbiesPattern, mediaDir, socialsPattern)
import MathiasSM.Rules.Favicon (faviconRules)
import System.FilePath.Posix (addTrailingPathSeparator, (</>))

-- | Processes all assets (media, styles, data files) into the final site
processAssets :: SiteConfig -> Rules ()
processAssets cfg = do
    processCss cfg
    processSvgImages cfg
    processDotImages cfg
    processFavicon cfg
    justCopy (mediaPattern cfg "photos" ["jpg", "png", "gif"]) (mediaRoute (mediaDir cfg))
    processDataFiles cfg

-- | Makes the data files (YAML) loadable by contexts and rules; they are not routed
processDataFiles :: SiteConfig -> Rules ()
processDataFiles cfg =
    match (hobbiesPattern cfg .||. socialsPattern cfg .||. experiencePattern cfg) $ compile getResourceString

-- | Compress all CSS as one file
processCss :: SiteConfig -> Rules ()
processCss cfg = do
    match (cssFiles cfg) (compile compressCssCompiler)
    create ["styles.css"] $ do
        route idRoute
        compile $ do
            css <- loadAll $ cssFiles cfg
            makeItem $ unlines $ map itemBody css

-- | Uses unix external filter (dot) to compile them as png
processDotImages :: SiteConfig -> Rules ()
processDotImages cfg = match (mediaPattern cfg "photos" ["dot"]) $ do
    route $ mediaRoute (mediaDir cfg) `composeRoutes` setExtension "png"
    compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

-- | Compiles branding and icons into context (for inlining), and copies them as images
processSvgImages :: SiteConfig -> Rules ()
processSvgImages cfg = do
    -- Allows including directly in html
    match svgFiles $ compile templateBodyCompiler
    -- Allows importing as img with src="<...>.svg"
    match svgFiles $
        version "svg" $ do
            route $ mediaRoute (mediaDir cfg) `composeRoutes` setExtension "svg"
            compile copyFileCompiler
  where
    svgFiles = foldr1 (.||.) [mediaPattern cfg "branding" ["svg"], mediaPattern cfg "icons" ["svg"]]

-- | Generates the favicons from the logo
processFavicon :: SiteConfig -> Rules ()
processFavicon cfg = do
    faviconRules $ fromString $ mediaDir cfg </> "branding" </> "logo.svg"

-- | Rule to copy files
justCopy :: Pattern -> Routes -> Rules ()
justCopy something routes = match something $
    version "raw" $ do
        route routes
        compile copyFileCompiler

-- | Where the site's own stylesheets live (see 'assetsDir')
cssFiles :: SiteConfig -> Pattern
cssFiles cfg = fromString $ assetsDir cfg </> "css" </> "*.css"

-- | Files with one of the extensions, anywhere under a media category
mediaPattern :: SiteConfig -> String -> [String] -> Pattern
mediaPattern cfg category extensions =
    foldr1 (.||.) [fromString $ mediaDir cfg </> category </> ("**." ++ ext) | ext <- extensions]

{- | Publishes `media/<category>/x` as `images/<category>/x`, except @branding@
which is published at the `images/` root (so the logo stays `images/logo.svg`).
-}
mediaRoute :: FilePath -> Routes
mediaRoute mediaDir' = customRoute $ \ident ->
    let path = toFilePath ident
        rest = fromMaybe path (stripPrefix (addTrailingPathSeparator mediaDir') path)
     in "images" </> fromMaybe rest (stripPrefix "branding/" rest)
