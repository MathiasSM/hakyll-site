-- | Build-time checks that content is consistent across files
module MathiasSM.Validate (ensureContentExists, validateHobbyPages, missingHobbyPages, validateNavEntries) where

import Control.Monad (filterM, unless)
import Data.List (intercalate)
import Hakyll (Rules, getAllMetadata, preprocess, toFilePath)
import MathiasSM.Config (contentDir, hobbiesTable, pagesPattern, postsPattern, requiredContent)
import MathiasSM.Content (isDraft, parseNavEntry)
import MathiasSM.Metadata (Key (Path), lookupKey)
import MathiasSM.Tsv (Row, parseTsv)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)

{- | Stops before building if the content repo is missing, or lacks a file the site needs

Hakyll would otherwise build whatever it finds, silently leaving out missing pages.
-}
ensureContentExists :: IO ()
ensureContentExists = do
  present <- doesDirectoryExist contentDir
  unless present
    $ die
    $ contentDir ++ "/ not found: clone the content repo there (see README)"
  missing <- filterM (fmap not . doesFileExist) requiredContent
  unless (null missing)
    $ die
    $ "Missing required content:\n" ++ unlines (map ("  " ++) missing)

{- | Fails the build, once, if any hobby (shown or not) lacks a published blog post with `path: <href>`

Runs when the rules are set up, not for every page that lists the hobbies.
-}
validateHobbyPages :: Rules ()
validateHobbyPages = do
  table <- preprocess $ readFile $ toFilePath hobbiesTable
  posts <- getAllMetadata $ postsPattern "blog"
  let paths = [path | (_, metadata) <- posts, not (isDraft metadata), Just path <- [lookupKey Path metadata]]
      missing = missingHobbyPages (parseTsv table) paths
  unless (null missing)
    $ preprocess
    $ ioError
    $ userError
    $ "hobbies.tsv: no post in "
      ++ contentDir
      ++ "/blog with a matching `path:` for: "
      ++ intercalate ", " missing

{- | Fails the build, once, listing every page whose menu entry (`nav:`) is invalid

Without this each page would report the same problem while rendering the menu.
-}
validateNavEntries :: Rules ()
validateNavEntries = do
  pages <- getAllMetadata $ pagesPattern "*"
  let problems = [toFilePath page ++ ": " ++ problem | (page, metadata) <- pages, Left found <- [parseNavEntry metadata], problem <- found]
  unless (null problems)
    $ preprocess
    $ ioError
    $ userError
    $ "Invalid menu entries:\n" ++ unlines (map ("  " ++) problems)

-- | The `href` of every hobby that no post `path` matches
missingHobbyPages :: [Row] -> [FilePath] -> [FilePath]
missingHobbyPages hobbies paths = [href | row <- hobbies, Just href <- [lookup "href" row], href `notElem` paths]
