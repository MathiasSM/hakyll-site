-- | The site's Atom feeds as a context, for the page head
module MathiasSM.Context.Feed (feedsContext) where

import Hakyll (Context, Item (Item, itemBody), field, fromFilePath, listField)
import MathiasSM.Config (SiteConfig)
import MathiasSM.Domain.Feed (Feed (..), feedHref, feeds)

-- | The site's Atom feeds as a `feeds` list (@title@, @href@), for the page head
feedsContext :: SiteConfig -> Context a
feedsContext cfg = listField "feeds" feedContext $ do
    found <- feeds cfg
    either (fail . unlines) (pure . map (\feed -> Item (fromFilePath $ feedFile feed) feed)) found
  where
    feedContext =
        field "title" (return . feedTitle . itemBody)
            <> field "href" (return . feedHref . itemBody)
