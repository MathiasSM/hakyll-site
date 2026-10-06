module MathiasSM.Context.Nav (navContext) where

import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Hakyll (
  Compiler,
  Context,
  Item (Item, itemBody, itemIdentifier),
  boolField,
  field,
  getMatches,
  listField,
  noResult,
  toFilePath,
 )
import MathiasSM.Config (pagesPattern)
import MathiasSM.Content (Language (En), NavEntry (..), languageCode, requireNav)
import System.FilePath.Posix (takeBaseName)

{- | The site menu as a `nav` list (`label`, `href`, `active`, `lang`), built from the
pages that declare `nav:` and ordered by `navOrder`

@current@ is the name of the page being rendered (its file name without extension, or
its post group); that menu entry is `active`.
-}
navContext :: String -> Context a
navContext current = listField "nav" entryContext navItems
 where
  entryContext =
    field "label" (return . navLabel . itemBody)
      <> field "href" (return . navHref . itemBody)
      <> boolField "active" ((== current) . takeBaseName . toFilePath . itemIdentifier)
      <> field "lang" (inOtherLanguage . navLanguage . itemBody)

  -- The menu sits in an English header, so only other languages need marking
  inOtherLanguage language
    | language == En = noResult "English"
    | otherwise = return $ languageCode language

navItems :: Compiler [Item NavEntry]
navItems = do
  pages <- getMatches $ pagesPattern "*"
  entries <- mapM (\page -> fmap (Item page) <$> requireNav page) pages
  return $ sortOn (navOrder . itemBody) $ catMaybes entries
