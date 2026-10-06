-- | Front matter keys the site relies on, and what each kind of content must provide
module MathiasSM.Metadata (
  Key (..),
  keyName,
  lookupKey,
  hasKeys,
  postMetadata,
  projectMetadata,
) where

import Data.Maybe (isJust)
import Hakyll (Metadata, lookupString)

{- | Every front matter key that content may use.

Add a constructor here (and in 'keyName') before using a new key anywhere;
see @scripts/get-all-frontmatter-keys.sh@ for the keys currently in content.
-}
data Key
  = -- Keys Hakyll itself interprets
    Title
  | Date
  | Published
  | -- Keys specific to this site
    Description
  | FinishDate
  | FinishedDate
  | HideDescription
  | Home
  | Href
  | Language
  | LastDate
  | LastModifiedAt
  | LongDescription
  | Path
  | Priority
  | Project
  | ShareDescription
  | ShareTitle
  | ShortDescription
  | StartDate
  | Status
  | Team
  | Templated
  | TOC
  | Type
  deriving (Eq, Show, Enum, Bounded)

-- | The key as spelled in front matter
keyName :: Key -> String
keyName key = case key of
  Title -> "title"
  Date -> "date"
  Published -> "published"
  Description -> "description"
  FinishDate -> "finishDate"
  FinishedDate -> "finishedDate"
  HideDescription -> "hideDescription"
  Home -> "home"
  Href -> "href"
  Language -> "language"
  LastDate -> "lastDate"
  LastModifiedAt -> "lastModifiedAt"
  LongDescription -> "longDescription"
  Path -> "path"
  Priority -> "priority"
  Project -> "project"
  ShareDescription -> "shareDescription"
  ShareTitle -> "shareTitle"
  ShortDescription -> "shortDescription"
  StartDate -> "startDate"
  Status -> "status"
  Team -> "team"
  Templated -> "templated"
  TOC -> "TOC"
  Type -> "type"

-- | Looks up a key's value as a string
lookupKey :: Key -> Metadata -> Maybe String
lookupKey = lookupString . keyName

-- | Checks that every given key is present
hasKeys :: [Key] -> Metadata -> Bool
hasKeys keys m = all (\k -> isJust $ lookupKey k m) keys

-- | What a post (blog, creative-writing) needs to be published
postMetadata :: Metadata -> Bool
postMetadata = hasKeys [Title, Date, Path]

-- | What a showcase project needs to be listed
projectMetadata :: Metadata -> Bool
projectMetadata = hasKeys [Title, Status, StartDate, ShortDescription, LongDescription]
