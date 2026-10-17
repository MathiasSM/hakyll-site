-- | Build-time checks that content is consistent across files
module MathiasSM.Validate (validateHobbyPages, missingHobbyPages, validateNavEntries) where

import Control.Monad (unless)
import Data.List (intercalate)
import Hakyll (Rules, getAllMetadata, preprocess, toFilePath)
import MathiasSM.Config (SiteConfig, contentDir, hobbiesTable, pagesPattern, postsPattern)
import MathiasSM.Content (isDraft, parseNavEntry)
import MathiasSM.Metadata (Key (Path), lookupKey)
import MathiasSM.Tsv (Row, parseTsv)

{- | Fails the build, once, if any hobby (shown or not) lacks a published post with `path: <href>`

Runs when the rules are set up, not for every page that lists the hobbies.
-}
validateHobbyPages :: SiteConfig -> Rules ()
validateHobbyPages cfg = do
    table <- preprocess $ readFile $ toFilePath (hobbiesTable cfg)
    posts <- getAllMetadata $ postsPattern cfg "*"
    let paths = [path | (_, metadata) <- posts, not (isDraft metadata), Just path <- [lookupKey Path metadata]]
        missing = missingHobbyPages (parseTsv table) paths
    unless (null missing)
        $ preprocess
        $ ioError
        $ userError
        $ "hobbies.tsv: no post in "
            ++ contentDir cfg
            ++ " with a matching `path:` for: "
            ++ intercalate ", " missing

{- | Fails the build, once, listing every page whose menu entry (`nav:`) is invalid

Without this each page would report the same problem while rendering the menu.
-}
validateNavEntries :: SiteConfig -> Rules ()
validateNavEntries cfg = do
    pages <- getAllMetadata $ pagesPattern cfg "*"
    let problems = [toFilePath page ++ ": " ++ problem | (page, metadata) <- pages, Left found <- [parseNavEntry metadata], problem <- found]
    unless (null problems)
        $ preprocess
        $ ioError
        $ userError
        $ "Invalid menu entries:\n" ++ unlines (map ("  " ++) problems)

-- | The @href@ of every hobby that no post @path@ matches
missingHobbyPages :: [Row] -> [FilePath] -> [FilePath]
missingHobbyPages hobbies paths = [href | row <- hobbies, Just href <- [lookup "href" row], href `notElem` paths]
