-- | The tables in content/tables, as lists usable in templates
module MathiasSM.Context.Tables (socialMediaContext, hobbiesContext, experienceContext) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Hakyll (
    Compiler,
    Context,
    Identifier,
    Item (Item, itemBody),
    field,
    fromFilePath,
    listField,
    load,
    toFilePath,
 )
import MathiasSM.Config (SiteConfig, experienceTable, hobbiesFile, mediaDir, socialsTable)
import MathiasSM.Content (Hobby (..), requireHobbies)
import MathiasSM.Tsv (Row, isYes, parseTsv, rowContext)
import System.FilePath.Posix ((</>))

-- | Table of social accounts (content/tables/socials.tsv)
socialMediaContext :: SiteConfig -> Context a
socialMediaContext cfg =
    mconcat
        [ socialsField "socials-all" (const True)
        , socialsField "socials-contact" (isYes "show_contact")
        , socialsField "socials-about" (isYes "show_about")
        ]
  where
    socialsField name keep =
        listField name rowCtx $ tableItems (socialsTable cfg) (filter keep)
    rowCtx = iconContext (mediaDir cfg) "images/icons/social/" "site"

-- | The hobbies, read from a single YAML list
hobbiesContext :: SiteConfig -> Context a
hobbiesContext cfg = listField "hobbies" (hobbyContext $ mediaDir cfg) $ hobbiesItems cfg

-- | The hobbies file's shown hobbies as items, in file order
hobbiesItems :: SiteConfig -> Compiler [Item Hobby]
hobbiesItems cfg = do
    file <- load (hobbiesFile cfg)
    hobbies <- requireHobbies file
    let shown = filter hobbyShow hobbies
    return [Item (fromFilePath $ toFilePath (hobbiesFile cfg) ++ "#" ++ show n) hobby | (n, hobby) <- zip [0 :: Int ..] shown]

-- | A hobby's fields as read by the hobbies template
hobbyContext :: FilePath -> Context Hobby
hobbyContext mediaDir' =
    mconcat
        [ hobbyField "name" hobbyName
        , hobbyField "href" hobbyHref
        , field "iconPath" (return . iconOf . itemBody)
        , field "iconSource" (return . sourceOf . itemBody)
        ]
  where
    hobbyField name get = field name (return . get . itemBody)
    iconOf hobby = "images/icons/" ++ hobbyIcon hobby ++ ".svg"
    sourceOf hobby = mediaDir' </> fromMaybe (iconOf hobby) (stripPrefix "images/" (iconOf hobby))

-- | Table of experience items
experienceContext :: SiteConfig -> Context a
experienceContext cfg = listField "experience" rowCtx $ tableItems (experienceTable cfg) id
  where
    rowCtx = iconContext (mediaDir cfg) "images/icons/" "icon"

{- | Row context with every column, plus the SVG named by the row's @column@ under
@prefix@ (a path inside `images/`): @iconPath@ is its URL path, @iconSource@ the
identifier to `$partial(...)$` it inline
-}
iconContext :: FilePath -> String -> String -> Context Row
iconContext mediaDir' prefix column =
    field "iconPath" (return . iconOf . itemBody)
        <> field "iconSource" (return . sourceOf . itemBody)
        <> rowContext
  where
    iconOf row = prefix ++ fromMaybe "" (lookup column row) ++ ".svg"
    sourceOf row = mediaDir' </> fromMaybe (iconOf row) (stripPrefix "images/" (iconOf row))

-- | Loads a TSV table as one item per (filtered) row
tableItems :: Identifier -> ([Row] -> [Row]) -> Compiler [Item Row]
tableItems path select = do
    table <- load path
    let rows = select $ parseTsv $ itemBody table
    return [Item (fromFilePath $ toFilePath path ++ "#" ++ show n) row | (n, row) <- zip [0 :: Int ..] rows]
