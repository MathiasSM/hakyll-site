module MathiasSM.Context (minimalCtx, navStateContext, postSocialTagsContext) where

import Control.Monad (unless)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import MathiasSM.Metadata (Key (Language, Path), lookupKey)
import MathiasSM.Tsv (Row, isYes, parseTsv, rowContext)
import Hakyll (
  Compiler,
  Context,
  Identifier,
  Item (Item, itemBody, itemIdentifier),
  fromFilePath,
  getMatches,
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

-- | Table of hobbies, as a list usable in templates
hobbiesContext :: Context a
hobbiesContext = listField "hobbies" rowCtx $ do
  ensureHobbyPagesExist
  tableItems "data/hobbies.tsv" (filter $ isYes "show")
 where
  rowCtx = field "iconPath" (return . iconOf . itemBody) <> rowContext
  iconOf row = "images/icons/" ++ fromMaybe "" (lookup "icon" row) ++ ".svg"

-- | Fails the build if any hobby (shown or not) lacks a blog post with `path: <href>`
ensureHobbyPagesExist :: Compiler ()
ensureHobbyPagesExist = do
  table <- load "data/hobbies.tsv"
  postIds <- getMatches "data/posts/blog/**"
  paths <- mapM (fmap (lookupKey Path) . getMetadata) postIds
  let hrefs = [h | row <- parseTsv $ itemBody table, Just h <- [lookup "href" row]]
      missing = filter (`notElem` catMaybes paths) hrefs
  unless (null missing) $
    fail $
      "hobbies.tsv: no post in data/posts/blog with a matching `path:` for: "
        ++ intercalate ", " missing

-- | Table of experience items, as a list usable in templates
experienceContext :: Context a
experienceContext = listField "experience" rowCtx $ tableItems "data/experience.tsv" id
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
    return $ fromMaybe defaultLanguage $ lookupKey Language metadata

  isLanguage lang item = do
    metaLang <- getLanguage item
    if metaLang == lang
      then return metaLang
      else noResult "No lang"

  defaultLanguage = "en"

-- TODO: date (published and modified) context with utc and pretty "ago" versions
