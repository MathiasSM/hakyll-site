{-# LANGUAGE Trustworthy #-}

module MathiasSM.Cli (cli) where

import Control.Monad (filterM, unless)
import Data.Maybe (fromMaybe)
import Hakyll (hakyllWith)
import MathiasSM.Config (Env (..), SiteConfig (..), requiredContent, siteConfiguration)
import MathiasSM.Rules (rules)
import MathiasSM.Website qualified as Web
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)

{- | Main entrypoint for the cli subcommands (usually called like @cabal run site SUBCMD@)

[@build@]: Generate the site into @_site@
[@clean@]: Clean the generated files from @_site@
[@rebuild@]: Runs @clean@ + @build@
[@server@]: Setup a quick dev server serving the files in @_site@
[@watch@]: Build, serve and rebuild site pieces as code changes
[@check@]: Verify broken links in @_site@ files

Note: build commands check for required content before running.
-}
cli :: IO ()
cli = do
    args <- getArgs
    dir <- fromMaybe "content" <$> lookupEnv "CONTENT_DIR"
    website <- Web.loadWebsite dir
    cfg <- loadSiteConfig dir website
    -- `clean` only removes generated files, so it works without the content
    unless ("clean" `elem` args) (ensureContentExists cfg)
    hakyllWith siteConfiguration (rules cfg website)

{- | Throws if the content repo is missing, or lacks a file the site needs

(Hakyll would otherwise build whatever it finds, possibly silently leaving out missing pages)
-}
ensureContentExists :: SiteConfig -> IO ()
ensureContentExists cfg = do
    present <- doesDirectoryExist $ contentDir cfg
    unless present
        $ die
        $ contentDir cfg ++ "/ not found"
    missing <- filterM (fmap not . doesFileExist) (requiredContent cfg)
    unless (null missing)
        $ die
        $ "Missing required content:\n" ++ unlines (map ("  " ++) missing)

{- | Builds the site configuration from @website.yaml@, with each identity field
overridable by an environment variable (e.g. @SITE_DOMAIN@ for a preview
deployment). @CONTENT_DIR@ is the only variable the builder reads directly; it
locates the content repo.
-}
loadSiteConfig :: FilePath -> Web.Website -> IO SiteConfig
loadSiteConfig dir website = do
    let site = Web.websiteSite website
    env <- siteEnv <$> lookupEnv "SITE_ENV"
    name <- override "SITE_NAME" (Web.siteName site)
    description <- override "SITE_DESCRIPTION" (Web.siteDescription site)
    author <- override "SITE_AUTHOR" (Web.siteAuthor site)
    year <- override "SITE_COPYRIGHT_YEAR" (Web.siteCopyrightYear site)
    domain <- override "SITE_DOMAIN" (Web.siteDomain site)
    twitter <- override "TWITTER_HANDLE" (Web.siteTwitterHandle site)
    pure
        SiteConfig
            { siteName = name
            , siteDescription = description
            , siteAuthor = author
            , siteCopyrightYear = year
            , siteDomain = domain
            , twitterHandle = twitter
            , contentDir = dir
            , siteEnv = env
            }
  where
    override var fallback = fromMaybe fallback <$> lookupEnv var

    siteEnv = maybe Prod (\value -> if value == "dev" then Dev else Prod)
