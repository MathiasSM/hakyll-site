-- | A page's language and its front-matter spelling
module Cozyll.Domain.Language (
    Language (..),
    languageCode,
    languageOf,
) where

import Cozyll.Metadata qualified as K
import Cozyll.Yaml (Check, enumFromJSON, optional)
import Data.Aeson (FromJSON (parseJSON))
import Data.Maybe (fromMaybe)
import Hakyll (Metadata)

-- | Language a page is written in
data Language = En | Es | Ja
    deriving (Eq, Show, Enum, Bounded)

-- | The code used in front matter and in @lang@ attributes
languageCode :: Language -> String
languageCode En = "en"
languageCode Es = "es"
languageCode Ja = "ja"

instance FromJSON Language where
    parseJSON = enumFromJSON "language" languageCode

-- | The language of an item (English unless stated)
languageOf :: Metadata -> Check Language
languageOf o = fromMaybe En <$> optional o K.Language
