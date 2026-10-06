module MathiasSM.Context.Language (languageContext) where

import Hakyll (Context, Item (itemIdentifier), field, noResult)
import MathiasSM.Content (languageCode, requireLanguage)

-- | Sets a language variable for choosing strings and using in html
languageContext :: Context a
languageContext =
  mconcat $
    field "language" (fmap languageCode . itemLanguage)
      : [field ("lang-" ++ languageCode lang) (isLanguage lang) | lang <- [minBound .. maxBound]]
 where
  itemLanguage = requireLanguage . itemIdentifier

  isLanguage lang item = do
    itemLang <- itemLanguage item
    if itemLang == lang
      then return $ languageCode lang
      else noResult "No lang"
