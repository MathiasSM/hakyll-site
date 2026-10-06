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

import Data.String (fromString)
import Hakyll (Configuration (ignoreFile), Identifier, Pattern, defaultConfiguration, fromFilePath)
import System.FilePath.Posix (takeFileName, (<.>), (</>))

-- | Site-wide information, exposed to templates as @site-name@, @site-description@, ...
siteName, siteDescription, siteAuthor, siteCopyrightYear :: String
siteName = "MathiasSM"
siteDescription = "Software Development Engineer"
siteAuthor = "Mathias San Miguel"
siteCopyrightYear = "2013"

-- | The site's domain, without scheme
siteDomain :: String
siteDomain = "mathiassm.dev"

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

-- | Where the content repo is cloned (git-ignored): pages/, projects/, tables/, and one folder per post group
contentDir :: FilePath
contentDir = "content"

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
