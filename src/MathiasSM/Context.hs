module MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext) where

import Control.Applicative (empty)
import Data.Maybe (fromMaybe)
import Hakyll

-- | "Minimal" context all pages should know about
minimalCtx :: Context String
minimalCtx =
  siteContext
    <> socialMediaContext
    <> languageContext
    <> defaultContext

{- | Given a string, builds a context field based on that name as currentView
TODO: Grab from item path?
-}
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
      [ constField "twitter-creator" "mathiassm"
      , constField "twitter-site" "mathiassm"
      , minimalCtx
      ]

-- | Sets site-wide information (site-<info>)
siteContext :: Context a
siteContext =
  mconcat
    [ constField "site-name" "MathiasSM"
    , constField "site-description" "Software Development Engineer"
    , constField "site-author" "Mathias San Miguel"
    , constField "site-copyrightYear" ("2013-" ++ show currentYear)
    , constField "site-baseUrl" "https://mathiassm.dev"
    ]
 where
  currentYear = 2021 -- TODO: Figure out how to pass this from IO

{- | Sets social media links as part of context (social-<platform>-href)
 TODO: Load from compiled source/md
-}
socialMediaContext :: Context a
socialMediaContext =
  mconcat $ hrefs ++ usernames ++ ctas
 where
  hrefs =
    [ constField "social-twitter-href" "https://twitter.com/mathiassm"
    , constField "social-github-href" "https://github.com/MathiasSM"
    , constField "social-linkedin-href" "https://www.linkedin.com/in/mathiassm/"
    , constField "social-mal-href" "https://myanimelist.net/animelist/mathiassm"
    , constField "social-imdb-href" "https://www.imdb.com/user/ur62639773/ratings"
    ]
  usernames =
    [ constField "social-twitter-username" "@mathiassm"
    , constField "social-github-username" "MathiasSM"
    , constField "social-linkedin-username" "mathiassmx/"
    , constField "social-mal-username" "mathiassm"
    , constField "social-imdb-username" "mathiassanmiguel"
    ]
  ctas =
    [ constField "social-twitter-cta" "Wanna chat?"
    , constField "social-github-cta" "Wanna code?"
    , constField "social-linkedin-cta" "Wanna hire?"
    , constField "social-mal-cta" "My Anime List"
    , constField "social-imdb-cta" "Movie ratings"
    ]

-- | Sets a language variable for choosing strings and using in html
languageContext :: Context a
languageContext =
  mconcat
    [ field "language" getLanguage
    , field "lang-es" (isLanguage "es")
    , field "lang-en" (isLanguage "en")
    ]
 where
  getLanguage item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe defaultLanguage $ lookupString "language" metadata

  isLanguage lang item = do
    metaLang <- getLanguage item
    if metaLang == lang
      then return metaLang
      else noResult "No lang"

  defaultLanguage = "en"

-- TODO: date (published and modified) context with utc and pretty "ago" versions
