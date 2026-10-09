{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoAllowAmbiguousTypes #-}
{-# LANGUAGE NoDeriveGeneric #-}

module MathiasSM.Cli (cli) where

import Control.Monad (unless)
import Hakyll (hakyllWith)
import MathiasSM.Config (siteConfiguration)
import MathiasSM.Site (rules)
import MathiasSM.Validate (ensureContentExists)
import System.Environment (getArgs)

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
    -- `clean` only removes generated files, so it works without the content
    unless ("clean" `elem` args) ensureContentExists
    hakyllWith siteConfiguration rules
