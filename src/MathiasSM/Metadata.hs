-- | Front matter keys the site knows about, and how they are spelled
module MathiasSM.Metadata (
  Key (..),
  keyName,
  lookupKey,
) where

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
  | Draft
  | EndDate
  | HideDescription
  | Home
  | Href
  | Language
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
  Draft -> "draft"
  EndDate -> "endDate"
  HideDescription -> "hideDescription"
  Home -> "home"
  Href -> "href"
  Language -> "language"
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
