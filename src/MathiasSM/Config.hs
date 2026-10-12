-- | Site-wide configuration: identity, where content lives, what groups exist, shared templates
module MathiasSM.Config (
    SiteConfig (..),
    baseUrl,
    siteConfiguration,
    requiredContent,
    assetsDir,
    postGroups,
    showcaseName,
    standalonePages,
    pagesPattern,
    postsPattern,
    postSnapshot,
    feedItemLimit,
    projectsPattern,
    tablesPattern,
    hobbiesTable,
    socialsTable,
    experienceTable,
    siteTemplate,
    minimalTemplate,
    pageTemplate,
    postTemplate,
)
where

import Data.String (fromString)
import Hakyll (Configuration (ignoreFile), Identifier, Pattern, Snapshot, defaultConfiguration, fromFilePath)
import System.FilePath.Posix (takeFileName, (<.>), (</>))

{- | Build-time site configuration, gathered once from the environment by
"MathiasSM.Cli". Every field maps to an environment variable; see @.env.example@
for the names and the values they default to.
-}
data SiteConfig = SiteConfig
    { siteName :: String
    -- ^ Site name, exposed to templates as @site-name@
    , siteDescription :: String
    -- ^ Site tagline, exposed as @site-description@
    , siteAuthor :: String
    -- ^ Site author, exposed as @site-author@
    , siteCopyrightYear :: String
    -- ^ Copyright year, exposed as @site-copyrightYear@
    , siteDomain :: String
    -- ^ Domain without scheme, exposed as @site-domain@
    , twitterHandle :: String
    -- ^ Twitter handle (without @@), used for the social cards of posts
    , contentDir :: FilePath
    -- ^ Where the content lives: pages/, projects/, tables/, and one folder per post group
    }

-- | Public origin of the site, without trailing slash
baseUrl :: SiteConfig -> String
baseUrl cfg = "https://" ++ siteDomain cfg

{- | Hakyll's defaults, except that `.well-known` folders are not ignored
(Hakyll skips every dot-folder, but `assets/static/.well-known/` must be published)
-}
siteConfiguration :: Configuration
siteConfiguration = defaultConfiguration{ignoreFile = ignore}
  where
    ignore path = takeFileName path /= ".well-known" && ignoreFile defaultConfiguration path

-- | Site-owned files: `css/`, `images/`, and `static/` (published at the site root as is)
assetsDir :: FilePath
assetsDir = "assets"

-- | Groups of posts, each with an index page (content/pages/<group>.*) and items (content/<group>/**)
postGroups :: [String]
postGroups = ["blog", "creative-writing"]

-- | Name of the page listing the projects in content/projects
showcaseName :: String
showcaseName = "showcase"

-- | Plain pages, each content/pages/<name>.*, routed by its `path:`
standalonePages :: [String]
standalonePages = ["about", "contact", "404"]

-- | Files the site cannot build without, relative to the repo root
requiredContent :: SiteConfig -> [FilePath]
requiredContent cfg =
    [contentDir cfg </> "pages" </> name <.> "md" | name <- standalonePages <> [showcaseName] <> postGroups]
        <> [contentDir cfg </> "tables" </> name <.> "tsv" | name <- ["hobbies", "socials", "experience"]]

-- | Standalone page named @name@
pagesPattern :: SiteConfig -> String -> Pattern
pagesPattern cfg name = fromString $ contentDir cfg </> "pages" </> name <.> "*"

-- | Items of a post group
postsPattern :: SiteConfig -> String -> Pattern
postsPattern cfg group = fromString $ contentDir cfg </> group </> "**"

-- | The snapshot holding each published post of a group, as rendered HTML (before the page templates)
postSnapshot :: String -> Snapshot
postSnapshot group = "published-" ++ group

-- | How many of the newest posts each Atom feed carries
feedItemLimit :: Int
feedItemLimit = 20

-- | All showcase projects
projectsPattern :: SiteConfig -> Pattern
projectsPattern cfg = fromString $ contentDir cfg </> "projects" </> "*.yaml"

-- | All tables (TSV)
tablesPattern :: SiteConfig -> Pattern
tablesPattern cfg = fromString $ contentDir cfg </> "tables" </> "*.tsv"

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
