-- | Build-time checks that content is consistent across files
module Cozyll.Rules.Validate (validateNavEntries) where

import Control.Monad (unless)
import Cozyll.Config (SiteConfig, pagesPattern)
import Cozyll.Domain.Nav (parseNavEntry)
import Hakyll (Rules, getAllMetadata, preprocess, toFilePath)

{- | Fails the build, once, listing every page whose menu entry (`nav:`) is invalid

Without this each page would report the same problem while rendering the menu.
-}
validateNavEntries :: SiteConfig -> Rules ()
validateNavEntries cfg = do
    pages <- getAllMetadata $ pagesPattern cfg "*"
    let problems = [toFilePath page ++ ": " ++ problem | (page, metadata) <- pages, Left found <- [parseNavEntry metadata], problem <- found]
    unless (null problems)
        $ preprocess
        $ ioError
        $ userError
        $ "Invalid menu entries:\n" ++ unlines (map ("  " ++) problems)
