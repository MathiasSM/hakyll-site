-- | The site menu, built from the pages that declare a nav entry
module MathiasSM.Context.Nav (navContext) where

import Control.Monad (filterM)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Hakyll (
    Compiler,
    Context,
    Item (Item, itemBody, itemIdentifier),
    boolField,
    field,
    getMatches,
    getMetadata,
    listField,
    noResult,
    toFilePath,
 )
import MathiasSM.Config (SiteConfig, pagesPattern, siteEnv)
import MathiasSM.Content (isHidden, requireNav)
import MathiasSM.Domain.Language (Language (En), languageCode)
import MathiasSM.Domain.Nav (NavEntry (..))
import System.FilePath.Posix (takeBaseName)

{- | The site menu as a @nav@ list (@label@, @href@, @active@, @lang@), built from the
pages that declare `nav:` and ordered by `navOrder`

@current@ is the name of the page being rendered (its file name without extension, or
its post group); that menu entry is @active@.
-}
navContext :: SiteConfig -> String -> Context a
navContext cfg current = listField "nav" entryContext (navItems cfg)
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

navItems :: SiteConfig -> Compiler [Item NavEntry]
navItems cfg = do
    pages <- getMatches $ pagesPattern cfg "*"
    visible <- filterM (fmap (not . isHidden (siteEnv cfg)) . getMetadata) pages
    entries <- mapM (\page -> fmap (Item page) <$> requireNav page) visible
    return $ sortOn (navOrder . itemBody) $ catMaybes entries
