-- | The context every page is rendered with, and the contexts that depend on it
module MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext) where

import Hakyll (
  Context,
  boolField,
  constField,
  defaultContext,
  jsonldField,
  openGraphField,
  twitterCardField,
 )
import MathiasSM.Config (twitterHandle)
import MathiasSM.Context.Feeds (feedsContext)
import MathiasSM.Context.Language (languageContext)
import MathiasSM.Context.Site (siteContext)
import MathiasSM.Context.Tables (experienceContext, hobbiesContext, socialMediaContext)

-- | "Minimal" context all pages should know about
minimalCtx :: Context String
minimalCtx =
  siteContext
    <> socialMediaContext
    <> hobbiesContext
    <> experienceContext
    <> languageContext
    <> feedsContext
    <> defaultContext

-- | Given a string, builds a context field based on that name as currentView
navStateContext :: String -> Context a
navStateContext currentView = boolField fieldName $ const True
 where
  fieldName = "currentview-" ++ currentView

-- | Sets HTML (as context) for article metadata
postSocialTagsContext :: Context String
postSocialTagsContext =
  mconcat
    [ twitterCardField "twitter" ctx
    , openGraphField "opengraph" ctx
    , jsonldField "jsonld" ctx
    ]
 where
  ctx =
    mconcat
      [ constField "twitter-creator" twitterHandle
      , constField "twitter-site" twitterHandle
      , minimalCtx
      ]
