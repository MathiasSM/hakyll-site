-- | The tables in content/tables, as lists usable in templates
module MathiasSM.Context.Tables (socialMediaContext, hobbiesContext, experienceContext) where

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
import MathiasSM.Config (SiteConfig, assetsDir, experienceTable, hobbiesTable, socialsTable)
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
    rowCtx = iconContext "images/icons/social/" "site"

-- | Table of hobbies
hobbiesContext :: SiteConfig -> Context a
hobbiesContext cfg = listField "hobbies" rowCtx $ tableItems (hobbiesTable cfg) (filter $ isYes "show")
  where
    rowCtx = iconContext "images/icons/" "icon"

-- | Table of experience items
experienceContext :: SiteConfig -> Context a
experienceContext cfg = listField "experience" rowCtx $ tableItems (experienceTable cfg) id
  where
    rowCtx = iconContext "images/icons/" "icon"

{- | Row context with every column, plus the SVG named by the row's @column@ under
@prefix@ (a path inside `images/`): `iconPath` is its URL path, `iconSource` the
identifier to `$partial(...)$` it inline
-}
iconContext :: String -> String -> Context Row
iconContext prefix column =
    field "iconPath" (return . iconOf . itemBody)
        <> field "iconSource" (return . (assetsDir </>) . iconOf . itemBody)
        <> rowContext
  where
    iconOf row = prefix ++ fromMaybe "" (lookup column row) ++ ".svg"

-- | Loads a TSV table as one item per (filtered) row
tableItems :: Identifier -> ([Row] -> [Row]) -> Compiler [Item Row]
tableItems path select = do
    table <- load path
    let rows = select $ parseTsv $ itemBody table
    return [Item (fromFilePath $ toFilePath path ++ "#" ++ show n) row | (n, row) <- zip [0 :: Int ..] rows]
