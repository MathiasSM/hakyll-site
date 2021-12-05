module MathiasSM.Metadata
  ( hasTitle,
    hasPublishedDate,
    hasModifiedDate,
    hasPath,
    hasTOC,
    hasStatus,
    hasHref,
    hasStartDate,
    hasDescriptions,
    metadataPublishedDate,
    metadataModifiedDate,
    metadataPath,
    metadataTOC,
    metadataLanguage,
  )
where

import Data.Maybe (fromJust, isJust)
import Hakyll (Metadata, lookupString, lookupStringList)

type MetadataKey = String

titleKey = "title"

publishedDateKey = "date"

modifiedDateKey = "lastModifiedAt"

pathKey = "path"

tocKey = "TOC"

languageKey = "language"

hrefKey = "href"

statusKey = "status"

startDateKey = "startDate"

shortDescriptionKey = "shortDescription"

longDescriptionKey = "longDescription"

---
hasMetadataString :: MetadataKey -> Metadata -> Bool
hasMetadataString field = isJust . lookupString field

hasMetadataStringList :: MetadataKey -> Metadata -> Bool
hasMetadataStringList field = isJust . lookupStringList field

metadataString :: MetadataKey -> Metadata -> String
metadataString field = fromJust . lookupString field

metadataStringList :: MetadataKey -> Metadata -> [String]
metadataStringList field = fromJust . lookupStringList field

---

type HasMetadataFieldF = Metadata -> Bool

hasTitle :: HasMetadataFieldF
hasTitle = hasMetadataString titleKey

hasPublishedDate :: HasMetadataFieldF
hasPublishedDate = hasMetadataString publishedDateKey

hasModifiedDate :: HasMetadataFieldF
hasModifiedDate = hasMetadataString publishedDateKey

hasPath :: HasMetadataFieldF
hasPath = hasMetadataString pathKey

hasTOC :: HasMetadataFieldF
hasTOC = hasMetadataString tocKey

hasHref :: HasMetadataFieldF
hasHref = hasMetadataString hrefKey

hasStartDate :: HasMetadataFieldF
hasStartDate = hasMetadataString startDateKey

hasStatus :: HasMetadataFieldF
hasStatus = hasMetadataString statusKey

hasDescriptions :: HasMetadataFieldF
hasDescriptions m = hasMetadataString shortDescriptionKey m && hasMetadataString longDescriptionKey m

---

type GetMetadateString = Metadata -> String

metadataLanguage :: GetMetadateString
metadataLanguage = metadataString languageKey

metadataPublishedDate :: GetMetadateString
metadataPublishedDate = metadataString publishedDateKey

metadataModifiedDate :: GetMetadateString
metadataModifiedDate = metadataString modifiedDateKey

metadataPath :: GetMetadateString
metadataPath = metadataString pathKey

metadataTOC :: GetMetadateString
metadataTOC = metadataString tocKey
