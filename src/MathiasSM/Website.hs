-- | The site configuration read from @website.yaml@ in the content repo
module MathiasSM.Website (
    Website (..),
    Site (..),
    Contact (..),
    Manifest (..),
    Funding (..),
    FundingEntity (..),
    FundingProject (..),
    FundingChannel (..),
    FundingPlan (..),
    Security (..),
    Robots (..),
    Trust (..),
    loadWebsite,
    validateSecurityExpiration,
)
where

import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day, addDays)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath.Posix ((</>))

-- | The @site:@ section: the site's identity
data Site = Site
    { siteName :: String
    , siteDescription :: String
    , siteAuthor :: String
    , siteCopyrightYear :: String
    , siteDomain :: String
    , siteTwitterHandle :: String
    }
    deriving (Eq, Show)

-- | The @contact:@ section: how to reach the site owner
data Contact = Contact
    { contactEmail :: String
    , contactPage :: String
    }
    deriving (Eq, Show)

-- | The @manifest:@ section: web app manifest colours
data Manifest = Manifest
    { manifestBackgroundColor :: String
    , manifestThemeColor :: String
    }
    deriving (Eq, Show)

-- | The @funding:@ section: data behind @funding.json@
data Funding = Funding
    { fundingEntity :: FundingEntity
    , fundingProjects :: [FundingProject]
    , fundingChannels :: [FundingChannel]
    , fundingPlans :: [FundingPlan]
    }
    deriving (Eq, Show)

-- | The entity being funded (the @funding.entity@ section)
data FundingEntity = FundingEntity
    { fundingEntityEmail :: String
    , fundingEntityDescription :: String
    }
    deriving (Eq, Show)

-- | A project listed in @funding.json@
data FundingProject = FundingProject
    { fundingProjectGuid :: String
    , fundingProjectName :: String
    , fundingProjectDescription :: String
    , fundingProjectRepositoryUrl :: String
    , fundingProjectRepositoryWellKnown :: String
    , fundingProjectLicenses :: [String]
    , fundingProjectTags :: [String]
    }
    deriving (Eq, Show)

-- | A funding channel in @funding.json@
data FundingChannel = FundingChannel
    { fundingChannelGuid :: String
    , fundingChannelType :: String
    , fundingChannelAddress :: String
    , fundingChannelDescription :: String
    }
    deriving (Eq, Show)

-- | A funding plan in @funding.json@
data FundingPlan = FundingPlan
    { fundingPlanGuid :: String
    , fundingPlanStatus :: String
    , fundingPlanName :: String
    , fundingPlanDescription :: String
    , fundingPlanAmount :: Int
    , fundingPlanCurrency :: String
    , fundingPlanFrequency :: String
    , fundingPlanChannels :: [String]
    }
    deriving (Eq, Show)

-- | The @security:@ section: data behind @.well-known/security.txt@
data Security = Security
    { securityPreferredLanguages :: String
    , securityExpires :: String
    , securityExpirationMarginDays :: Int
    }
    deriving (Eq, Show)

-- | The @robots:@ section: data behind @robots.txt@
data Robots = Robots {robotsDisallow :: [String]}
    deriving (Eq, Show)

-- | The @trust:@ section: data behind @.well-known/trust.txt@
data Trust = Trust {trustTrainingAllowed :: String}
    deriving (Eq, Show)

-- | Everything @website.yaml@ describes
data Website = Website
    { websiteSite :: Site
    , websiteContact :: Contact
    , websiteManifest :: Manifest
    , websiteFunding :: Funding
    , websiteSecurity :: Security
    , websiteRobots :: Robots
    , websiteTrust :: Trust
    }
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

instance FromJSON Contact where
    parseJSON = withObject "contact" $ \o ->
        Contact <$> o .: "email" <*> o .: "page"

instance FromJSON Manifest where
    parseJSON = withObject "manifest" $ \o ->
        Manifest <$> o .: "backgroundColor" <*> o .: "themeColor"

instance FromJSON Funding where
    parseJSON = withObject "funding" $ \o ->
        Funding
            <$> o .: "entity"
            <*> o .: "projects"
            <*> o .: "channels"
            <*> o .: "plans"

instance FromJSON FundingEntity where
    parseJSON = withObject "funding.entity" $ \o ->
        FundingEntity <$> o .: "email" <*> o .: "description"

instance FromJSON FundingProject where
    parseJSON = withObject "funding.project" $ \o ->
        FundingProject
            <$> o .: "guid"
            <*> o .: "name"
            <*> o .: "description"
            <*> o .: "repositoryUrl"
            <*> o .: "repositoryWellKnown"
            <*> o .: "licenses"
            <*> o .: "tags"

instance FromJSON FundingChannel where
    parseJSON = withObject "funding.channel" $ \o ->
        FundingChannel
            <$> o .: "guid"
            <*> o .: "type"
            <*> o .: "address"
            <*> o .: "description"

instance FromJSON FundingPlan where
    parseJSON = withObject "funding.plan" $ \o ->
        FundingPlan
            <$> o .: "guid"
            <*> o .: "status"
            <*> o .: "name"
            <*> o .: "description"
            <*> o .: "amount"
            <*> o .: "currency"
            <*> o .: "frequency"
            <*> o .: "channels"

instance FromJSON Security where
    parseJSON = withObject "security" $ \o ->
        Security <$> o .: "preferredLanguages" <*> o .: "expires" <*> o .: "expirationMarginDays"

instance FromJSON Robots where
    parseJSON = withObject "robots" $ \o -> Robots <$> o .: "disallow"

instance FromJSON Trust where
    parseJSON = withObject "trust" $ \o -> Trust <$> o .: "trainingAllowed"

instance FromJSON Website where
    parseJSON = withObject "website" $ \o ->
        Website
            <$> o .: "site"
            <*> o .: "contact"
            <*> o .: "manifest"
            <*> o .: "funding"
            <*> o .: "security"
            <*> o .: "robots"
            <*> o .: "trust"

-- | Reads and parses @<contentDir>/website.yaml@, failing with a clear message
loadWebsite :: FilePath -> IO Website
loadWebsite contentDir = do
    let path = contentDir </> "website.yaml"
    exists <- doesFileExist path
    unless exists $ die $ path ++ " not found"
    result <- decodeFileEither path
    either (\e -> die $ "Invalid " ++ path ++ ":\n" ++ prettyPrintParseException e) pure result

-- | Fails if the @security.txt@ expiration is within the configured margin (or already passed)
validateSecurityExpiration :: Website -> IO ()
validateSecurityExpiration website = do
    today <- utctDay <$> getCurrentTime
    let security = websiteSecurity website
        margin = securityExpirationMarginDays security
        deadline = addDays (fromIntegral margin) today
    case parseExpiryDate (securityExpires security) of
        Nothing -> die $ "Invalid security.expires date: " ++ securityExpires security
        Just expires ->
            when (expires <= deadline) $
                die $ "security.txt expires on " ++ show expires ++ " (within " ++ show margin ++ " days); renew it"

-- | Parses the leading @YYYY-MM-DD@ of an ISO timestamp
parseExpiryDate :: String -> Maybe Day
parseExpiryDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . take 10
