-- | Site-wide constants: where content lives, what groups exist, shared templates
module MathiasSM.Config (
  baseUrl,
  postGroups,
  showcaseName,
  pagesPattern,
  postsPattern,
  projectsPattern,
  hobbiesTable,
  socialsTable,
  experienceTable,
  siteTemplate,
  minimalTemplate,
  pageTemplate,
  postTemplate,
) where

import Data.String (fromString)
import Hakyll (Identifier, Pattern)

-- | Public origin of the site, without trailing slash
baseUrl :: String
baseUrl = "https://mathiassm.dev"

-- | Groups of posts, each with an index page (data/pages/<group>.*) and items (data/posts/<group>/**)
postGroups :: [String]
postGroups = ["blog", "escritos"]

-- | Name of the page listing the projects in data/projects
showcaseName :: String
showcaseName = "showcase"

-- | Standalone page named @name@
pagesPattern :: String -> Pattern
pagesPattern name = fromString $ "data/pages/" ++ name ++ ".*"

-- | Items of a post group
postsPattern :: String -> Pattern
postsPattern group = fromString $ "data/posts/" ++ group ++ "/**"

-- | All showcase projects
projectsPattern :: Pattern
projectsPattern = "data/projects/**"

-- | Tables (TSV) read by contexts
hobbiesTable, socialsTable, experienceTable :: Identifier
hobbiesTable = "data/hobbies.tsv"
socialsTable = "data/socials.tsv"
experienceTable = "data/experience.tsv"

-- | Templates every kind of page goes through
siteTemplate, minimalTemplate, pageTemplate, postTemplate :: Identifier
siteTemplate = "templates/site.html"
minimalTemplate = "templates/minimal.html"
pageTemplate = "templates/as-page.html"
postTemplate = "templates/as-post.html"
