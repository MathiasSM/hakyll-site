-- | Discovers the site's writings: its groups and standalone pages
module MathiasSM.Writings (discoverGroups, discoverStandalonePages) where

import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Hakyll (MonadMetadata (getAllMetadata), toFilePath)
import MathiasSM.Config (SiteConfig (contentDir), pagesPattern, writingsPattern)
import MathiasSM.Content (groupOf, isIndex)
import System.FilePath.Posix (takeBaseName)

-- | The distinct groups across @writings/@, from `group:` front matter or the folder a post sits in
discoverGroups :: (MonadMetadata m) => SiteConfig -> m [String]
discoverGroups cfg = do
    pages <- getAllMetadata (writingsPattern cfg)
    pure $ sort $ nub $ mapMaybe (\(ident, metadata) -> groupOf (contentDir cfg) ident metadata) pages

-- | The standalone pages: flat @writings/@ files that are not group indexes
discoverStandalonePages :: (MonadMetadata m) => SiteConfig -> m [String]
discoverStandalonePages cfg = do
    pages <- getAllMetadata (pagesPattern cfg "*")
    pure $ sort [takeBaseName (toFilePath ident) | (ident, metadata) <- pages, not (isIndex metadata)]
