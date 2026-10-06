module MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext) where

import Data.Maybe (fromMaybe)
import MathiasSM.Tsv (Row, isYes, parseTsv, rowContext)
import Hakyll (
  Compiler,
  Context,
  Identifier,
  Item (Item, itemBody),
  fromFilePath,
  toFilePath,
  listField,
  load,
  Item (itemIdentifier),
  MonadMetadata (getMetadata),
  boolField,
  constField,
  defaultContext,
  field,
  jsonldField,
  lookupString,
  noResult,
  openGraphField,
  twitterCardField,
 )

-- | "Minimal" context all pages should know about
minimalCtx :: Context String
minimalCtx =
  siteContext
    <> socialMediaContext
    <> hobbiesContext
    <> languageContext
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
    , constField "site-copyrightYear" "2013"
    , constField "site-baseUrl" "https://mathiassm.dev"
    , constField "root" "https://mathiassm.dev"
    ]

-- | Table of social accounts (data/socials.tsv), as lists usable in templates
socialMediaContext :: Context a
socialMediaContext =
  mconcat
    [ socialsField "socials-all" (const True)
    , socialsField "socials-contact" (isYes "show_contact")
    , socialsField "socials-about" (isYes "show_about")
    ]
 where
  socialsField name keep =
    listField name rowCtx $ tableItems "data/socials.tsv" (filter keep)
  rowCtx = field "iconPath" (return . iconOf . itemBody) <> rowContext
  iconOf row = "images/icons/social/" ++ fromMaybe "" (lookup "site" row) ++ ".svg"

-- | Table of hobbies (data/hobbies.tsv), as a list usable in templates
hobbiesContext :: Context a
hobbiesContext = listField "hobbies" rowCtx $ tableItems "data/hobbies.tsv" id
 where
  rowCtx = field "iconPath" (return . iconOf . itemBody) <> rowContext
  iconOf row = "images/icons/" ++ fromMaybe "" (lookup "icon" row) ++ ".svg"

-- | Loads a TSV table as one item per (filtered) row
tableItems :: Identifier -> ([Row] -> [Row]) -> Compiler [Item Row]
tableItems path select = do
  table <- load path
  let rows = select $ parseTsv $ itemBody table
  return [Item (fromFilePath $ toFilePath path ++ "#" ++ show n) row | (n, row) <- zip [0 :: Int ..] rows]

-- | Sets a language variable for choosing strings and using in html
languageContext :: Context a
languageContext =
  mconcat
    [ field "language" getLanguage
    , field "lang-es" (isLanguage "es")
    , field "lang-en" (isLanguage "en")
    , field "lang-jp" (isLanguage "jp")
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
