module MathiasSM.Metadata (
  HasMetadata,
  hasDescriptions, hasHref, hasLanguage, hasLongDescription, hasMetadataStringList,
  hasModifiedDate, hasPath, hasPublishedDate, hasShortDescription, hasStartDate, hasStatus, hasTOC,
  hasTitle,
  matchesHref, matchesLanguage, matchesLongDescription, matchesMetadataStringList,
  matchesModifiedDate, matchesPath, matchesPublishedDate, matchesShortDescription,
  matchesStartDate, matchesStatus, matchesTOC, matchesTitle,
  metadataHref, metadataLanguage, metadataLongDescription, metadataModifiedDate, metadataPath,
  metadataPublishedDate, metadataShortDescription, metadataStartDate, metadataStatus,
  metadataStringList, metadataTOC, metadataTitle,
)
where

import Data.Maybe (fromJust, isJust)
import Hakyll (Metadata, lookupString, lookupStringList)

type MetadataKey = String

hrefKey, languageKey, longDescriptionKey, modifiedDateKey, pathKey
  , publishedDateKey, shortDescriptionKey, startDateKey, statusKey
  , titleKey, tocKey
  :: MetadataKey

hrefKey             = "href"
languageKey         = "language"
longDescriptionKey  = "longDescription"
modifiedDateKey     = "lastModifiedAt"
pathKey             = "path"
publishedDateKey    = "date"
shortDescriptionKey = "shortDescription"
startDateKey        = "startDate"
statusKey           = "status"
titleKey            = "title"
tocKey              = "TOC"

---


type GetMetadata a = Metadata -> a
type HasMetadata = Metadata -> Bool
type MatchesMetadata a = (a -> Bool) -> Metadata -> Bool

metadataString :: MetadataKey -> GetMetadata String
metadataString field = fromJust . lookupString field

metadataStringList :: MetadataKey -> GetMetadata [String]
metadataStringList field = fromJust . lookupStringList field

hasMetadataString :: MetadataKey -> HasMetadata
hasMetadataString field = isJust . lookupString field

hasMetadataStringList :: MetadataKey -> HasMetadata
hasMetadataStringList field = isJust . lookupStringList field

matchesMetadataString :: MetadataKey -> MatchesMetadata String
matchesMetadataString field predicate = maybe False predicate . lookupString field

matchesMetadataStringList :: MetadataKey -> MatchesMetadata [String]
matchesMetadataStringList field predicate = maybe False predicate . lookupStringList field

---

hasHref, hasLanguage, hasLongDescription, hasModifiedDate, hasPath
  , hasPublishedDate, hasShortDescription, hasStartDate, hasStatus
  , hasTitle, hasTOC
  :: HasMetadata

metadataHref, metadataLanguage, metadataLongDescription, metadataModifiedDate, metadataPath
  , metadataPublishedDate, metadataShortDescription, metadataStartDate, metadataStatus
  , metadataTitle, metadataTOC
  :: GetMetadata String

matchesHref, matchesLanguage, matchesLongDescription, matchesModifiedDate, matchesPath
  , matchesPublishedDate, matchesShortDescription, matchesStartDate, matchesStatus
  , matchesTitle, matchesTOC
  :: MatchesMetadata String

hasHref                  = hasMetadataString hrefKey
hasLanguage              = hasMetadataString languageKey
hasLongDescription       = hasMetadataString longDescriptionKey
hasModifiedDate          = hasMetadataString modifiedDateKey
hasPath                  = hasMetadataString pathKey
hasPublishedDate         = hasMetadataString publishedDateKey
hasShortDescription      = hasMetadataString shortDescriptionKey
hasStartDate             = hasMetadataString startDateKey
hasStatus                = hasMetadataString statusKey
hasTOC                   = hasMetadataString tocKey
hasTitle                 = hasMetadataString titleKey

metadataHref             = metadataString hrefKey
metadataLanguage         = metadataString languageKey
metadataLongDescription  = metadataString longDescriptionKey
metadataModifiedDate     = metadataString modifiedDateKey
metadataPath             = metadataString pathKey
metadataPublishedDate    = metadataString publishedDateKey
metadataShortDescription = metadataString shortDescriptionKey
metadataStartDate        = metadataString startDateKey
metadataStatus           = metadataString statusKey
metadataTOC              = metadataString tocKey
metadataTitle            = metadataString titleKey

matchesHref              = matchesMetadataString hrefKey
matchesLanguage          = matchesMetadataString languageKey
matchesLongDescription   = matchesMetadataString longDescriptionKey
matchesModifiedDate      = matchesMetadataString modifiedDateKey
matchesPath              = matchesMetadataString pathKey
matchesPublishedDate     = matchesMetadataString publishedDateKey
matchesShortDescription  = matchesMetadataString shortDescriptionKey
matchesStartDate         = matchesMetadataString startDateKey
matchesStatus            = matchesMetadataString statusKey
matchesTOC               = matchesMetadataString tocKey
matchesTitle             = matchesMetadataString titleKey

--- Special

hasDescriptions :: Metadata -> Bool
hasDescriptions m = hasMetadataString shortDescriptionKey m && hasMetadataString longDescriptionKey m
