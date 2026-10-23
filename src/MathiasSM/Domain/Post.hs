-- | A post's front-matter model and parser
module MathiasSM.Domain.Post (
    Post (..),
    parsePost,
    parsePostYaml,
) where

import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
import Data.Time (Day)
import Hakyll (Metadata)
import MathiasSM.Domain.Language (Language, languageOf)
import MathiasSM.Metadata qualified as K
import MathiasSM.Yaml (Check (runCheck), decodeYaml, optional, required)

-- | What a post (blog, creative-writing) needs to be published
data Post = Post
    { postTitle :: String
    , postDate :: Day
    , postPath :: FilePath
    , postLanguage :: Language
    , postLastModified :: Maybe Day
    , postDraft :: Bool
    }
    deriving (Eq, Show)

-- | Parses front matter, reporting every problem (not just the first)
parsePost :: Metadata -> Either [String] Post
parsePost o = do
    post <-
        runCheck $
            Post
                <$> required o K.Title
                <*> required o K.Date
                <*> required o K.Path
                <*> languageOf o
                <*> optional o K.LastModifiedAt
                <*> (fromMaybe False <$> optional o K.Draft)
    case postLastModified post of
        Just modified
            | modified < postDate post -> Left ["lastModifiedAt: is before date"]
        _ -> Right post

-- | 'parsePost' on YAML text (the same shape as front matter)
parsePostYaml :: String -> Either [String] Post
parsePostYaml = decodeYaml >=> parsePost
