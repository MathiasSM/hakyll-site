-- | Site-wide information (name, description, author, URLs) as template fields
module MathiasSM.Context.Site (siteContext) where

import Hakyll (Context, constField)
import MathiasSM.Config (SiteConfig, baseUrl, mathjaxVersion, siteAuthor, siteCopyrightYear, siteDescription, siteDomain, siteName)

-- | Sets site-wide information (site-<info>)
siteContext :: SiteConfig -> Context a
siteContext cfg =
    mconcat
        [ constField "site-name" (siteName cfg)
        , constField "site-description" (siteDescription cfg)
        , constField "site-author" (siteAuthor cfg)
        , constField "site-copyrightYear" (show $ siteCopyrightYear cfg)
        , constField "site-baseUrl" (baseUrl cfg)
        , constField "site-domain" (siteDomain cfg)
        , constField "mathjax-version" (mathjaxVersion cfg)
        , constField "root" (baseUrl cfg) -- read by Hakyll's own social cards
        ]
