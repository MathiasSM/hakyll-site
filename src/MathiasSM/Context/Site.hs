module MathiasSM.Context.Site (siteContext) where

import Hakyll (Context, constField)
import MathiasSM.Config (
  baseUrl,
  siteAuthor,
  siteCopyrightYear,
  siteDescription,
  siteName,
 )

-- | Sets site-wide information (site-<info>)
siteContext :: Context a
siteContext =
  mconcat
    [ constField "site-name" siteName
    , constField "site-description" siteDescription
    , constField "site-author" siteAuthor
    , constField "site-copyrightYear" siteCopyrightYear
    , constField "site-baseUrl" baseUrl
    , constField "root" baseUrl -- read by Hakyll's own social cards
    ]
