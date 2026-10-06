module MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext) where

import Data.Maybe (fromMaybe)
import MathiasSM.Config (baseUrl, experienceTable, hobbiesTable, socialsTable)
import MathiasSM.Metadata (Key (Language), lookupKey)
import MathiasSM.Tsv (Row, isYes, parseTsv, rowContext)
import MathiasSM.Validate (ensureHobbyPagesExist)
import Hakyll (
  Compiler,
  Context,
  Identifier,
  Item (Item, itemBody, itemIdentifier),
  fromFilePath,
  toFilePath,
  listField,
  load,
  MonadMetadata (getMetadata),
  boolField,
  constField,
  defaultContext,
  field,
  jsonldField,
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
    <> experienceContext
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
    , constField "site-baseUrl" baseUrl
    , constField "root" baseUrl -- read by Hakyll's own social cards
    ]

-- | Table of social accounts (content/tables/socials.tsv), as lists usable in templates
socialMediaContext :: Context a
socialMediaContext =
  mconcat
    [ socialsField "socials-all" (const True)
    , socialsField "socials-contact" (isYes "show_contact")
    , socialsField "socials-about" (isYes "show_about")
    ]
 where
  socialsField name keep =
    listField name rowCtx $ tableItems socialsTable (filter keep)
  rowCtx = iconContext "images/icons/social/" "site"

-- | Table of hobbies, as a list usable in templates
hobbiesContext :: Context a
hobbiesContext = listField "hobbies" rowCtx $ do
  ensureHobbyPagesExist
  tableItems hobbiesTable (filter $ isYes "show")
 where
  rowCtx = iconContext "images/icons/" "icon"

-- | Table of experience items, as a list usable in templates
experienceContext :: Context a
experienceContext = listField "experience" rowCtx $ tableItems experienceTable id
 where
  rowCtx = iconContext "images/icons/" "icon"

{- | Row context with every column, plus `iconPath`: the SVG named by the row's
@column@ under @prefix@
-}
iconContext :: String -> String -> Context Row
iconContext prefix column = field "iconPath" (return . iconOf . itemBody) <> rowContext
 where
  iconOf row = prefix ++ fromMaybe "" (lookup column row) ++ ".svg"

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
    return $ fromMaybe defaultLanguage $ lookupKey Language metadata

  isLanguage lang item = do
    metaLang <- getLanguage item
    if metaLang == lang
      then return metaLang
      else noResult "No lang"

  defaultLanguage = "en"

-- TODO: date (published and modified) context with utc and pretty "ago" versions
