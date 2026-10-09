-- | Site-wide constants: where content lives, what groups exist, shared templates
module MathiasSM.Config (
  siteName,
  siteDescription,
  siteAuthor,
  siteCopyrightYear,
  siteDomain,
  baseUrl,
  twitterHandle,
  siteConfiguration,
  contentDir,
  assetsDir,
  postGroups,
  showcaseName,
  standalonePages,
  requiredContent,
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
) where

import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Hakyll (Configuration (ignoreFile), Identifier, Pattern, Snapshot, defaultConfiguration, fromFilePath)
import System.Environment (lookupEnv)
import System.FilePath.Posix (takeFileName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | Site-wide information, exposed to templates as @site-name@, @site-description@, ...
siteName, siteDescription, siteAuthor, siteCopyrightYear :: String
siteName = "MathiasSM"
siteDescription = "Software Development Engineer"
siteAuthor = "Mathias San Miguel"
siteCopyrightYear = "2013"

-- | The site's domain, without scheme.
--
-- Overridable at build time via the @SITE_DOMAIN@ environment variable (CI uses
-- this to build the gamma/preview deployment on a different domain). Read once,
-- when the site executable starts.
{-# NOINLINE siteDomain #-}
siteDomain :: String
siteDomain = fromMaybe "mathiassm.dev" (unsafePerformIO (lookupEnv "SITE_DOMAIN"))

-- | Public origin of the site, without trailing slash
baseUrl :: String
baseUrl = "https://" ++ siteDomain

-- | Twitter handle (without @) used for the social cards of posts
twitterHandle :: String
twitterHandle = "mathiassm"

{- | Hakyll's defaults, except that `.well-known` folders are not ignored
(Hakyll skips every dot-folder, but `assets/static/.well-known/` must be published)
-}
siteConfiguration :: Configuration
siteConfiguration = defaultConfiguration{ignoreFile = ignore}
 where
  ignore path = takeFileName path /= ".well-known" && ignoreFile defaultConfiguration path

-- | Where the content lives: pages/, projects/, tables/, and one folder per post group.
--
-- Overridable at build time via the @CONTENT_DIR@ environment variable
-- (web-writings builds with @CONTENT_DIR=.@ so it compiles its own checkout
-- directly). Read once, when the site executable starts.
{-# NOINLINE contentDir #-}
contentDir :: FilePath
contentDir = fromMaybe "content" (unsafePerformIO (lookupEnv "CONTENT_DIR"))

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
requiredContent :: [FilePath]
requiredContent =
  [contentDir </> "pages" </> name <.> "md" | name <- standalonePages <> [showcaseName] <> postGroups]
    <> [contentDir </> "tables" </> name <.> "tsv" | name <- ["hobbies", "socials", "experience"]]

-- | Standalone page named @name@
pagesPattern :: String -> Pattern
pagesPattern name = fromString $ contentDir </> "pages" </> name <.> "*"

-- | Items of a post group
postsPattern :: String -> Pattern
postsPattern group = fromString $ contentDir </> group </> "**"

-- | The snapshot holding each published post of a group, as rendered HTML (before the page templates)
postSnapshot :: String -> Snapshot
postSnapshot group = "published-" ++ group

-- | How many of the newest posts each Atom feed carries
feedItemLimit :: Int
feedItemLimit = 20

-- | All showcase projects
projectsPattern :: Pattern
projectsPattern = fromString $ contentDir </> "projects" </> "*.yaml"

-- | All tables (TSV)
tablesPattern :: Pattern
tablesPattern = fromString $ contentDir </> "tables" </> "*.tsv"

-- | Tables (TSV) read by contexts
hobbiesTable, socialsTable, experienceTable :: Identifier
hobbiesTable = fromFilePath $ contentDir </> "tables" </> "hobbies.tsv"
socialsTable = fromFilePath $ contentDir </> "tables" </> "socials.tsv"
experienceTable = fromFilePath $ contentDir </> "tables" </> "experience.tsv"

-- | Templates every kind of page goes through
siteTemplate, minimalTemplate, pageTemplate, postTemplate :: Identifier
siteTemplate = "templates/site.html"
minimalTemplate = "templates/minimal.html"
pageTemplate = "templates/as-page.html"
postTemplate = "templates/as-post.html"
