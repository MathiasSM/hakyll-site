-- | The context every page is rendered with, and the contexts that depend on it
module Cozyll.Context (globalContext, postSocialTagsContext) where

import Cozyll.Config (SiteConfig, twitterHandle)
import Cozyll.Context.Data (dataContext)
import Cozyll.Context.Feed (feedsContext)
import Cozyll.Context.Language (languageContext)
import Cozyll.Context.Site (siteContext)
import Cozyll.Context.Social (socialMediaContext)
import Data.List (isInfixOf)
import Hakyll (
    Context,
    Item (itemBody),
    boolField,
    constField,
    defaultContext,
    jsonldField,
    openGraphField,
    twitterCardField,
 )

-- | Every field available to all pages and list items
globalContext :: SiteConfig -> Context String
globalContext cfg =
    siteContext cfg
        <> socialMediaContext cfg
        <> dataContext cfg
        <> languageContext
        <> feedsContext cfg
        <> mathContext
        <> defaultContext

-- | True when the page's rendered body contains math, so templates can load MathJax
mathContext :: Context String
mathContext =
    boolField "math" $ \item ->
        "class=\"math " `isInfixOf` itemBody item

-- | Sets HTML (as context) for article metadata
postSocialTagsContext :: SiteConfig -> Context String
postSocialTagsContext cfg =
    mconcat
        [ twitterCardField "twitter" ctx
        , openGraphField "opengraph" ctx
        , jsonldField "jsonld" ctx
        ]
  where
    ctx =
        mconcat
            [ constField "twitter-creator" (twitterHandle cfg)
            , constField "twitter-site" (twitterHandle cfg)
            , globalContext cfg
            ]
