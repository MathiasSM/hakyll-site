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
import MathiasSM.Config (experienceTable, hobbiesTable, socialsTable)
import MathiasSM.Tsv (Row, isYes, parseTsv, rowContext)

-- | Table of social accounts (content/tables/socials.tsv)
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

-- | Table of hobbies
hobbiesContext :: Context a
hobbiesContext = listField "hobbies" rowCtx $ tableItems hobbiesTable (filter $ isYes "show")
 where
  rowCtx = iconContext "images/icons/" "icon"

-- | Table of experience items
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
