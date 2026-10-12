-- | The site configuration read from @website.yaml@ in the content repo
module MathiasSM.Website (Website (..), Site (..), loadWebsite) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath.Posix ((</>))

-- | The @site:@ section of @website.yaml@: the site's identity
data Site = Site
    { siteName :: String
    , siteDescription :: String
    , siteAuthor :: String
    , siteCopyrightYear :: String
    , siteDomain :: String
    , siteTwitterHandle :: String
    }
    deriving (Eq, Show)

-- | Everything @website.yaml@ describes (identity now; more sections later)
data Website = Website {websiteSite :: Site}
    deriving (Eq, Show)

instance FromJSON Site where
    parseJSON = withObject "site" $ \o ->
        Site
            <$> o .: "name"
            <*> o .: "description"
            <*> o .: "author"
            <*> o .: "copyrightYear"
            <*> o .: "domain"
            <*> o .: "twitterHandle"

instance FromJSON Website where
    parseJSON = withObject "website" $ \o -> Website <$> o .: "site"

-- | Reads and parses @<contentDir>/website.yaml@, failing with a clear message
loadWebsite :: FilePath -> IO Website
loadWebsite contentDir = do
    let path = contentDir </> "website.yaml"
    exists <- doesFileExist path
    unless exists $ die $ path ++ " not found"
    result <- decodeFileEither path
    either (\e -> die $ "Invalid " ++ path ++ ":\n" ++ prettyPrintParseException e) pure result
