{-# LANGUAGE Trustworthy #-}

module MathiasSM.Cli (cli) where

import Control.Monad (filterM, unless)
import Data.Maybe (fromMaybe)
import Hakyll (hakyllWith)
import MathiasSM.Config (SiteConfig (..), requiredContent, siteConfiguration)
import MathiasSM.Rules (rules)
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
    cfg <- loadSiteConfig
    -- `clean` only removes generated files, so it works without the content
    unless ("clean" `elem` args) (ensureContentExists cfg)
    hakyllWith siteConfiguration (rules cfg)

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

{- | Reads the site configuration from the environment, failing with a clear
message if any required variable is missing.
-}
loadSiteConfig :: IO SiteConfig
loadSiteConfig =
    SiteConfig
        <$> require "SITE_NAME"
        <*> require "SITE_DESCRIPTION"
        <*> require "SITE_AUTHOR"
        <*> require "SITE_COPYRIGHT_YEAR"
        <*> require "SITE_DOMAIN"
        <*> require "TWITTER_HANDLE"
        <*> (fromMaybe "content" <$> lookupEnv "CONTENT_DIR")
  where
    require name = lookupEnv name >>= maybe (die $ "Missing required environment variable " ++ name ++ " (see .env.example)") pure
