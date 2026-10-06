-- | The Atom feeds: one per post group, described by the group's index page
module MathiasSM.Feeds (Feed (..), feedHref, feeds, absoluteUrl) where

import Data.Either (lefts, rights)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Hakyll (MonadMetadata (getMatches, getMetadata), toFilePath)
import MathiasSM.CleanURL (outputPath)
import MathiasSM.Config (pagesPattern, postGroups, siteName)
import MathiasSM.Metadata (Key (Description, Path, Title), lookupKey)
import System.FilePath.Posix (takeDirectory, (</>))

data Feed = Feed
  { feedGroup :: String
  , feedTitle :: String
  , feedDescription :: String
  , feedFile :: FilePath
  -- ^ Output file, relative to the site root (e.g. @blog/atom.xml@)
  }
  deriving (Eq, Show)

-- | Where the feed is served from
feedHref :: Feed -> String
feedHref feed = '/' : feedFile feed

{- | The feed of every post group, or every problem found

Each feed lives next to the group's index page (`/blog` has `/blog/atom.xml`) and
takes its title and description from that page.
-}
feeds :: (MonadMetadata m) => m (Either [String] [Feed])
feeds = collect <$> mapM feedOf postGroups
 where
  collect results = case lefts results of
    [] -> Right (rights results)
    problems -> Left problems

feedOf :: (MonadMetadata m) => String -> m (Either String Feed)
feedOf group = do
  indexPages <- getMatches (pagesPattern group)
  case indexPages of
    [] -> pure $ Left $ "no index page for the group " ++ group
    page : _ -> do
      metadata <- getMetadata page
      pure $ case lookupKey Path metadata of
        Nothing -> Left $ toFilePath page ++ ": needs a `path:` for the feed"
        Just path ->
          Right
            Feed
              { feedGroup = group
              , feedTitle = fromMaybe group (lookupKey Title metadata) ++ " - " ++ siteName
              , feedDescription = fromMaybe "" (lookupKey Description metadata)
              , feedFile = takeDirectory (outputPath path) </> "atom.xml"
              }

{- | Makes a root-relative URL (`/contact`) absolute, as feed readers have no page to resolve it against.

Anything else (full URLs, `//host`, `#anchor`, `mailto:`, relative paths) is left alone.
-}
absoluteUrl :: String -> String -> String
absoluteUrl root url
  | "/" `isPrefixOf` url && not ("//" `isPrefixOf` url) = root ++ url
  | otherwise = url
