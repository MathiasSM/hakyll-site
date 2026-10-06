module MathiasSM.Context.Feeds (feedsContext) where

import Hakyll (Context, Item (Item, itemBody), field, fromFilePath, listField)
import MathiasSM.Feeds (Feed (..), feedHref, feeds)

-- | The site's Atom feeds as a `feeds` list (`title`, `href`), for the page head
feedsContext :: Context a
feedsContext = listField "feeds" feedContext $ do
  found <- feeds
  either (fail . unlines) (pure . map (\feed -> Item (fromFilePath $ feedFile feed) feed)) found
 where
  feedContext =
    field "title" (return . feedTitle . itemBody)
      <> field "href" (return . feedHref . itemBody)
