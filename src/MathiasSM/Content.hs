-- | Typed views of the content files: posts (front matter) and projects (YAML)
module MathiasSM.Content (
  Language (..),
  languageCode,
  Status (..),
  statusName,
  Post (..),
  Project (..),
  isDraft,
  aliasesOf,
  parseAliasesYaml,
  parsePost,
  parsePostYaml,
  parseProject,
  parseProjectYaml,
  showcaseKey,
  requirePost,
  requireLanguage,
  requireProject,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value (Null, String), withText)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.List (intercalate, stripPrefix)
import Data.Ord (Down (Down))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (Day)
import Data.Yaml (decodeEither', prettyPrintParseException)
import Hakyll (Compiler, Identifier, Item (itemBody, itemIdentifier), Metadata, getMetadata, getUnderlying, toFilePath)
import MathiasSM.Metadata (Key, keyName)
import qualified MathiasSM.Metadata as K
import Control.Monad ((>=>))
import Prelude hiding (lookup)

-- | Language a page is written in
data Language = En | Es | Jp
  deriving (Eq, Show, Enum, Bounded)

-- | The code used in front matter and in `lang` attributes
languageCode :: Language -> String
languageCode En = "en"
languageCode Es = "es"
languageCode Jp = "jp"

instance FromJSON Language where
  parseJSON = enumFromJSON "language" languageCode

-- | How far along a project is
data Status = Alpha | Finished | Ongoing | Unmaintained
  deriving (Eq, Show, Enum, Bounded)

statusName :: Status -> String
statusName Alpha = "alpha"
statusName Finished = "finished"
statusName Ongoing = "ongoing"
statusName Unmaintained = "unmaintained"

instance FromJSON Status where
  parseJSON = enumFromJSON "status" statusName

-- | Accepts exactly the spellings 'render' produces
enumFromJSON :: (Enum a, Bounded a) => String -> (a -> String) -> Value -> Parser a
enumFromJSON what render = withText what $ \t ->
  case [x | x <- [minBound .. maxBound], render x == T.unpack t] of
    x : _ -> pure x
    [] ->
      fail $
        "unknown " ++ what ++ " \"" ++ T.unpack t ++ "\" (expected "
          ++ intercalate ", " (map render [minBound .. maxBound]) ++ ")"

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

-- | What a showcase project needs to be listed
data Project = Project
  { projectTitle :: String
  , projectHref :: String
  , projectStatus :: Status
  , projectStart :: Day
  , projectEnd :: Maybe Day
  , projectShortDescription :: String
  , projectLongDescription :: String
  , projectTeam :: [String]
  , projectPriority :: Maybe Int
  }
  deriving (Eq, Show)

-- | Parses front matter, reporting every problem (not just the first)
parsePost :: Metadata -> Either [String] Post
parsePost o =
  runCheck $
    Post
      <$> required o K.Title
      <*> required o K.Date
      <*> required o K.Path
      <*> languageOf o
      <*> optional o K.LastModifiedAt
      <*> (fromMaybe False <$> optional o K.Draft)

-- | The language of an item (English unless stated)
languageOf :: Metadata -> Check Language
languageOf o = fromMaybe En <$> optional o K.Language

-- | Whether an item is marked `draft: true`, and so left out of the site
isDraft :: Metadata -> Bool
isDraft o = runCheck (optional o K.Draft) == Right (Just True)

{- | Old paths an item should redirect from (`aliases: [/old]`, or a single `aliases: /old`)

Each must be an absolute path other than the home page.
-}
aliasesOf :: Metadata -> Either [String] [FilePath]
aliasesOf o = do
  aliases <- maybe [] unAliasList <$> runCheck (optional o K.Aliases)
  case concatMap problem aliases of
    [] -> Right aliases
    problems -> Left problems
 where
  problem alias
    | alias == "/" = ["aliases: / is the home page, it can't be an alias"]
    | take 1 alias /= "/" = ["aliases: " ++ show alias ++ " must start with / (like /old-name)"]
    | otherwise = []

-- | A string or a list of strings
newtype AliasList = AliasList {unAliasList :: [String]}

instance FromJSON AliasList where
  parseJSON v@(String _) = AliasList . pure <$> parseJSON v
  parseJSON v = AliasList <$> parseJSON v

-- | 'aliasesOf' on YAML text (the same shape as front matter)
parseAliasesYaml :: String -> Either [String] [FilePath]
parseAliasesYaml = decodeYaml >=> aliasesOf

-- | Parses a project, reporting every problem (not just the first)
parseProject :: Object -> Either [String] Project
parseProject o =
  runCheck $
    Project
      <$> required o K.Title
      <*> required o K.Href
      <*> required o K.Status
      <*> required o K.StartDate
      <*> optional o K.EndDate
      <*> required o K.ShortDescription
      <*> required o K.LongDescription
      <*> (fromMaybe [] <$> optional o K.Team)
      <*> optional o K.Priority

{- | Sort key for the showcase: projects with a `priority` first (lowest number
first), then the rest, newest start first
-}
showcaseKey :: Project -> (Int, Down Day)
showcaseKey p = (fromMaybe maxBound (projectPriority p), Down (projectStart p))

-- | 'parsePost' on YAML text (the same shape as front matter)
parsePostYaml :: String -> Either [String] Post
parsePostYaml = decodeYaml >=> parsePost

-- | 'parseProject' on the contents of a project file
parseProjectYaml :: String -> Either [String] Project
parseProjectYaml = decodeYaml >=> parseProject

decodeYaml :: String -> Either [String] Object
decodeYaml = first (\e -> [prettyPrintParseException e]) . decodeEither' . T.encodeUtf8 . T.pack

-- | Parses the current item's front matter, failing the build with its file name
requirePost :: Compiler Post
requirePost = do
  ident <- getUnderlying
  metadata <- getMetadata ident
  orFail ident (parsePost metadata)

-- | The language of an item, failing the build with its file name if it's not a known one
requireLanguage :: Identifier -> Compiler Language
requireLanguage ident = do
  metadata <- getMetadata ident
  orFail ident (runCheck $ languageOf metadata)

-- | Parses a project item (its body is the YAML), failing the build with its file name
requireProject :: Item String -> Compiler Project
requireProject item = orFail (itemIdentifier' item) (parseProjectYaml $ itemBody item)
 where
  itemIdentifier' = itemIdentifier

orFail :: Identifier -> Either [String] a -> Compiler a
orFail ident = either (fail . message) pure
 where
  message problems = toFilePath ident ++ ":\n" ++ unlines (map ("  " ++) problems)

-- Accumulating validation ----------------------------------------------------

-- | Like 'Either', but combining failures instead of stopping at the first
newtype Check a = Check {runCheck :: Either [String] a}

instance Functor Check where
  fmap f (Check r) = Check (fmap f r)

instance Applicative Check where
  pure = Check . Right
  Check (Left a) <*> Check (Left b) = Check (Left (a ++ b))
  Check (Left a) <*> Check _ = Check (Left a)
  Check (Right f) <*> Check r = Check (fmap f r)

required :: FromJSON a => Object -> Key -> Check a
required o key = case lookup (fromString $ keyName key) o of
  Nothing -> Check $ Left [keyName key ++ ": missing"]
  Just Null -> Check $ Left [keyName key ++ ": missing"]
  Just v -> Check $ decodeField key v

optional :: FromJSON a => Object -> Key -> Check (Maybe a)
optional o key = case lookup (fromString $ keyName key) o of
  Nothing -> pure Nothing
  Just Null -> pure Nothing
  Just v -> Check $ Just <$> decodeField key v

decodeField :: FromJSON a => Key -> Value -> Either [String] a
decodeField key = first (\e -> [keyName key ++ ": " ++ clean e]) . parseEither parseJSON
 where
  clean e = fromMaybe e (stripPrefix "Error in $: " e)
