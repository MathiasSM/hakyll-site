module MathiasSM.Rules.Feed (processFeeds) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Hakyll (
  Context,
  FeedConfiguration (..),
  Item (itemBody, itemIdentifier),
  Rules,
  compile,
  create,
  defaultContext,
  field,
  fromFilePath,
  idRoute,
  loadAllSnapshots,
  loadBody,
  mapContext,
  preprocess,
  recentFirst,
  renderAtomWithTemplates,
  route,
  urlField,
  withUrls,
 )
import MathiasSM.CleanURL (cleanIndex)
import MathiasSM.Config (baseUrl, feedItemLimit, postSnapshot, postsPattern, siteAuthor)
import MathiasSM.Content (Post (..), requirePostOf)
import MathiasSM.Feeds (Feed (..), absoluteUrl, feeds)

-- | Builds the Atom feed of every post group: its newest posts, in full
processFeeds :: Rules ()
processFeeds = do
  found <- feeds
  case found of
    Left problems -> preprocess $ ioError $ userError $ "Invalid feeds:\n" ++ unlines (map ("  " ++) problems)
    Right all' -> forM_ all' $ \feed ->
      create [fromFilePath $ feedFile feed] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots (postsPattern $ feedGroup feed) (postSnapshot $ feedGroup feed)
          feedTemplate <- loadBody "templates/atom-feed.xml"
          itemTemplate <- loadBody "templates/atom-item.xml"
          renderAtomWithTemplates feedTemplate itemTemplate (configuration feed) itemContext (take feedItemLimit posts)

configuration :: Feed -> FeedConfiguration
configuration feed =
  FeedConfiguration
    { feedTitle = MathiasSM.Feeds.feedTitle feed
    , feedDescription = MathiasSM.Feeds.feedDescription feed
    , feedAuthorName = siteAuthor
    , feedAuthorEmail = ""
    , feedRoot = baseUrl
    }

{- | What each entry reads: its clean URL, its full text with absolute links
(`description`, the feed templates' name for the content), and its dates
-}
itemContext :: Context String
itemContext =
  mapContext cleanIndex (urlField "url")
    <> field "description" (return . withUrls (absoluteUrl baseUrl) . itemBody)
    <> timestampField "published" postDate
    <> timestampField "updated" (\post -> fromMaybe (postDate post) (postLastModified post))
    <> defaultContext

-- | An RFC 3339 timestamp (midnight UTC) of one of the post's dates
timestampField :: String -> (Post -> Day) -> Context String
timestampField name pick = field name $ \item ->
  formatTime defaultTimeLocale "%Y-%m-%dT00:00:00Z" . pick <$> requirePostOf (itemIdentifier item)
