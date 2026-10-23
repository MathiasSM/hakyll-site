-- | A menu entry's front-matter model and parser
module Cozyll.Domain.Nav (
    NavEntry (..),
    parseNavEntry,
    parseNavYaml,
) where

import Control.Monad ((>=>))
import Cozyll.Domain.Language (Language, languageOf)
import Cozyll.Metadata qualified as K
import Cozyll.Yaml (Check (runCheck), decodeYaml, optional, required)
import Hakyll (Metadata)

-- | A page that appears in the site menu (`nav: <label>`, `navOrder: <n>` in its front matter)
data NavEntry = NavEntry
    { navLabel :: String
    , navOrder :: Int
    , navHref :: FilePath
    , navLanguage :: Language
    }
    deriving (Eq, Show)

{- | The menu entry a page asks for, if it has a `nav:` label (it then also needs `navOrder`
and @path@)
-}
parseNavEntry :: Metadata -> Either [String] (Maybe NavEntry)
parseNavEntry o = do
    label <- runCheck (optional o K.Nav)
    case label of
        Nothing -> Right Nothing
        Just text ->
            runCheck $
                (\order path language -> Just (NavEntry text order path language))
                    <$> required o K.NavOrder
                    <*> required o K.Path
                    <*> languageOf o

-- | 'parseNavEntry' on YAML text (the same shape as front matter)
parseNavYaml :: String -> Either [String] (Maybe NavEntry)
parseNavYaml = decodeYaml >=> parseNavEntry
