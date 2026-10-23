-- | The site's Atom feeds as a context, for the page head
module Cozyll.Context.Feed (feedsContext) where

import Cozyll.Config (SiteConfig)
import Cozyll.Domain.Feed (Feed (..), feedHref, feeds)
import Hakyll (Context, Item (Item, itemBody), field, fromFilePath, listField)

-- | The site's Atom feeds as a `feeds` list (@title@, @href@), for the page head
feedsContext :: SiteConfig -> Context a
feedsContext cfg = listField "feeds" feedContext $ do
    found <- feeds cfg
    either (fail . unlines) (pure . map (\feed -> Item (fromFilePath $ feedFile feed) feed)) found
  where
    feedContext =
        field "title" (return . feedTitle . itemBody)
            <> field "href" (return . feedHref . itemBody)
