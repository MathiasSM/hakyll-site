-- | Site-wide configuration: identity, where content lives, what groups exist, shared templates
module MathiasSM.Config
  ( -- * Hakyll's 'Configuration'
    siteConfiguration,
    -- * Deployment config
    SiteConfig (..),
    Env (..),
    baseUrl,
    -- * Files
    requiredContent,
    mediaDir,
    projectsGroup,
    postSnapshot,
    -- * 'Pattern's for files
    writingsPattern,
    pagesPattern,
    postsPattern,
    projectsPattern,
    tablesPattern,
    -- * Identifiers
    -- ** Templates
    siteTemplate,
    minimalTemplate,
    pageTemplate,
    postTemplate,
    -- ** Tables
    hobbiesTable,
    socialsTable,
    experienceTable,
  )
where

import Data.String (fromString)
import Hakyll (Configuration (ignoreFile), Identifier, Pattern, Snapshot, defaultConfiguration, fromFilePath)
import System.FilePath.Posix (takeFileName, (<.>), (</>))

-- | The build environment: 'Dev' also publishes the preview pages under @/example/@
data Env = Dev | Prod
  deriving (Eq, Show)

-- | Build-time site configuration, gathered once from the environment by "MathiasSM.Cli".
-- Every field maps to an environment variable; see @.env.example@
data SiteConfig = SiteConfig
  { -- | Site name, exposed to templates as @site-name@
    siteName :: String,
    -- | Site tagline, exposed as @site-description@
    siteDescription :: String,
    -- | Site author, exposed as @site-author@
    siteAuthor :: String,
    -- | Copyright year, exposed as @site-copyrightYear@
    siteCopyrightYear :: Int,
    -- | Domain without scheme, exposed as @site-domain@
    siteDomain :: String,
    -- | Twitter (X) handle (without \@), used for the social cards of posts
    twitterHandle :: String,
    -- | MathJax CDN version (from @mathjax:@ in @website.yaml@), exposed as @mathjax-version@
    --
    -- I might some day decide not to use the CDN and rather vend the bundle.
    mathjaxVersion :: String,
    -- | Where the actual content lives
    contentDir :: FilePath,
    -- | Where the assests live (@/assets/@ by default)
    assetsDir :: FilePath,
    -- | Where the templates live (@/templates/@ by default)
    templatesDir :: FilePath,
    -- | Whether this is a dev build (which publishes @/example/@ preview pages)
    siteEnv :: Env
  }

-- | Hakyll's default configuration, except that `.well-known` folders are not ignored
-- (Hakyll skips every dot-folder, but `assets/static/.well-known/` must be published)
-- TODO: Review if condition is correct, or even needed
siteConfiguration :: Configuration
siteConfiguration = defaultConfiguration {ignoreFile = ignore}
  where
    ignore path = takeFileName path /= ".well-known" && ignoreFile defaultConfiguration path


-- | Public origin of the site, without trailing slash
baseUrl :: SiteConfig -> String
baseUrl cfg = "https://" ++ siteDomain cfg


-- | Where the site's media lives: `branding/`, `icons/`, `photos/` (published at `images/`)
mediaDir :: SiteConfig -> FilePath
mediaDir cfg = contentDir cfg </> "media"

-- | Currently hardcoded files the site cannot build without, relative to the repo root
requiredContent :: SiteConfig -> [FilePath]
requiredContent cfg =
  [contentDir cfg </> "tables" </> name <.> "tsv" | name <- ["hobbies", "socials", "experience"]]


-- | The group whose items are the projects (its index page lists `projects/`)
projectsGroup :: String
projectsGroup = "showcase"


-- | Every file under 'contentDir'@/writings/@
writingsPattern :: SiteConfig -> Pattern
writingsPattern cfg = fromString $ contentDir cfg </> "writings" </> "**"

-- | A standalone page or group index page (a flat file under 'contentDir'@/writings/@) named @name@
pagesPattern :: SiteConfig -> String -> Pattern
pagesPattern cfg name = fromString $ contentDir cfg </> "writings" </> name <.> "*"

-- | Items of a post group
postsPattern :: SiteConfig -> String -> Pattern
postsPattern cfg group = fromString $ contentDir cfg </> "writings" </> group </> "**"

-- | All showcase projects
projectsPattern :: SiteConfig -> Pattern
projectsPattern cfg = fromString $ contentDir cfg </> "projects" </> "*.yaml"

-- | All tables (TSV)
tablesPattern :: SiteConfig -> Pattern
tablesPattern cfg = fromString $ contentDir cfg </> "tables" </> "*.tsv"


-- | The snapshot holding each published post of a single group, as rendered HTML (before templates)
postSnapshot :: String -> Snapshot
postSnapshot group = "published-" ++ group


-- | Tables (TSV) read by contexts
hobbiesTable, socialsTable, experienceTable :: SiteConfig -> Identifier
hobbiesTable cfg = fromFilePath $ contentDir cfg </> "tables" </> "hobbies.tsv"
socialsTable cfg = fromFilePath $ contentDir cfg </> "tables" </> "socials.tsv"
experienceTable cfg = fromFilePath $ contentDir cfg </> "tables" </> "experience.tsv"

-- | Templates every kind of page goes through
siteTemplate, minimalTemplate, pageTemplate, postTemplate :: Identifier
siteTemplate = "templates/site.html"
minimalTemplate = "templates/minimal.html"
pageTemplate = "templates/as-page.html"
postTemplate = "templates/as-post.html"

