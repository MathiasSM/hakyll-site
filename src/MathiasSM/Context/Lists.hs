-- | The data files (socials, hobbies, experience), as lists usable in templates
module MathiasSM.Context.Lists (socialMediaContext, hobbiesContext, experienceContext) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Hakyll (
    Compiler,
    Context,
    Identifier,
    Item (Item, itemBody),
    field,
    fromFilePath,
    listField,
    load,
    toFilePath,
 )
import MathiasSM.Config (SiteConfig, experienceFile, hobbiesFile, mediaDir, socialsFile)
import MathiasSM.Content (requireExperiences, requireHobbies, requireSocials)
import MathiasSM.Yaml.Experience (Experience (..))
import MathiasSM.Yaml.Hobby (Hobby (..))
import MathiasSM.Yaml.Social (Social (..))
import System.FilePath.Posix ((</>))

-- | The social accounts, in the lists read by the social templates
socialMediaContext :: SiteConfig -> Context a
socialMediaContext cfg =
    mconcat
        [ socialsField "socials-all" (const True)
        , socialsField "socials-contact" socialShowContact
        , socialsField "socials-about" socialShowAbout
        ]
  where
    socialsField name keep =
        listField name (socialContext $ mediaDir cfg) $ listItems (socialsFile cfg) requireSocials keep

-- | The hobbies, read from a single YAML list
hobbiesContext :: SiteConfig -> Context a
hobbiesContext cfg = listField "hobbies" (hobbyContext $ mediaDir cfg) $ listItems (hobbiesFile cfg) requireHobbies hobbyShow

-- | The experience items, read from a single YAML list
experienceContext :: SiteConfig -> Context a
experienceContext cfg = listField "experience" (experienceItemContext $ mediaDir cfg) $ listItems (experienceFile cfg) requireExperiences (const True)

-- | A social account's fields, as read by the social templates
socialContext :: FilePath -> Context Social
socialContext mediaDir' =
    iconContext mediaDir' "images/icons/social/" socialIcon
        <> field "href" (return . socialHref . itemBody)
        <> field "username" (return . socialUsername . itemBody)

-- | A hobby's fields, as read by the hobbies template
hobbyContext :: FilePath -> Context Hobby
hobbyContext mediaDir' =
    iconContext mediaDir' "images/icons/" hobbyIcon
        <> field "name" (return . hobbyName . itemBody)
        <> field "href" (return . hobbyHref . itemBody)

-- | An experience item's fields, as read by the experience template
experienceItemContext :: FilePath -> Context Experience
experienceItemContext mediaDir' =
    iconContext mediaDir' "images/icons/" experienceIcon
        <> field "name" (return . experienceName . itemBody)

{- | The SVG named by each item's icon under @prefix@ (a path inside `images/`):
@iconPath@ is its URL path, @iconSource@ the identifier to `$partial(...)$` it inline
-}
iconContext :: FilePath -> String -> (a -> String) -> Context a
iconContext mediaDir' prefix getIcon =
    field "iconPath" (return . pathOf . itemBody)
        <> field "iconSource" (return . sourceOf . itemBody)
  where
    pathOf item = prefix ++ getIcon item ++ ".svg"
    sourceOf item = mediaDir' </> fromMaybe (pathOf item) (stripPrefix "images/" (pathOf item))

-- | Loads a YAML list file as one item per kept element
listItems :: Identifier -> (Item String -> Compiler [a]) -> (a -> Bool) -> Compiler [Item a]
listItems path require keep = do
    file <- load path
    items <- require file
    let kept = filter keep items
    return [Item (fromFilePath $ toFilePath path ++ "#" ++ show n) item | (n, item) <- zip [0 :: Int ..] kept]
