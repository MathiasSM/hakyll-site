module MathiasSM.Rules (rules) where

import Data.List (sort)
import Hakyll (Rules, compile, getAllMetadata, match, templateBodyCompiler, toFilePath)
import MathiasSM.Config (SiteConfig, pagesPattern, postGroups, showcaseName)
import MathiasSM.Content (isIndex)
import MathiasSM.Rules.Assets (processAssets)
import MathiasSM.Rules.Feed (processFeeds)
import MathiasSM.Rules.PostGroup (processPostGroup)
import MathiasSM.Rules.Redirects (processRedirects)
import MathiasSM.Rules.Showcase (processShowcase)
import MathiasSM.Rules.SinglePages (page, processPage)
import MathiasSM.Rules.Sitemap (processSitemap)
import MathiasSM.Rules.Static (processStaticFiles)
import MathiasSM.Validate (validateHobbyPages, validateNavEntries)
import MathiasSM.Website (Website)
import System.FilePath.Posix (takeBaseName)

rules :: SiteConfig -> Website -> Rules ()
rules cfg website = do
    match "templates/*" $ compile templateBodyCompiler
    validateHobbyPages cfg
    validateNavEntries cfg
    processAssets cfg
    processStaticFiles cfg website
    standalone <- discoverStandalonePages cfg
    mapM_ (processPage cfg . page cfg) standalone
    processShowcase cfg showcaseName
    mapM_ (processPostGroup cfg) (postGroups cfg)
    processFeeds cfg
    processSitemap cfg
    processRedirects cfg

-- | The standalone pages: flat writings files that are neither group indexes nor the showcase
discoverStandalonePages :: SiteConfig -> Rules [String]
discoverStandalonePages cfg = do
    pages <- getAllMetadata (pagesPattern cfg "*")
    pure
        $ sort
            [ takeBaseName (toFilePath ident)
            | (ident, metadata) <- pages
            , not (isIndex metadata)
            , takeBaseName (toFilePath ident) /= showcaseName
            ]
