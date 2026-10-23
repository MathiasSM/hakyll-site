-- | A menu entry's front-matter model and parser
module MathiasSM.Domain.Nav (
    NavEntry (..),
    parseNavEntry,
    parseNavYaml,
) where

import Control.Monad ((>=>))
import Hakyll (Metadata)
import MathiasSM.Domain.Language (Language, languageOf)
import MathiasSM.Metadata qualified as K
import MathiasSM.Yaml (Check (runCheck), decodeYaml, optional, required)

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
