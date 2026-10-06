module MathiasSM.Context.Site (siteContext) where

import Hakyll (Context, constField)
import MathiasSM.Config (
  baseUrl,
  siteAuthor,
  siteCopyrightYear,
  siteDescription,
  siteDomain,
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
    , constField "site-domain" siteDomain
    , constField "root" baseUrl -- read by Hakyll's own social cards
    ]
