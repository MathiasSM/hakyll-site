-- | The context every page is rendered with, and the contexts that depend on it
module MathiasSM.Context (minimalCtx, postSocialTagsContext) where

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
import MathiasSM.Config (SiteConfig, twitterHandle)
import MathiasSM.Context.Feeds (feedsContext)
import MathiasSM.Context.Language (languageContext)
import MathiasSM.Context.Site (siteContext)
import MathiasSM.Context.Tables (experienceContext, hobbiesContext, socialMediaContext)

-- | "Minimal" context all pages should know about
minimalCtx :: SiteConfig -> Context String
minimalCtx cfg =
    siteContext cfg
        <> socialMediaContext cfg
        <> hobbiesContext cfg
        <> experienceContext cfg
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
            , minimalCtx cfg
            ]
